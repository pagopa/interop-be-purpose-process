package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.server.directives.FileInfo
import cats.implicits._
import it.pagopa.interop.authorizationmanagement.client.model.ClientComponentState
import it.pagopa.interop.catalogmanagement.client.model.EService
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposemanagement.client.model.PurposeVersionState._
import it.pagopa.interop.purposemanagement.client.model._
import it.pagopa.interop.purposeprocess.api.impl.Ownership.{CONSUMER, PRODUCER, SELF_CONSUMER}
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.{EServiceInfo, LanguageIt}
import it.pagopa.interop.purposeprocess.service.AgreementManagementService.OPERATIVE_AGREEMENT_STATES
import it.pagopa.interop.tenantmanagement.client.model.TenantKind
import it.pagopa.interop.purposeprocess.service._

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

final case class PurposeVersionActivation(
  agreementManagementService: AgreementManagementService,
  authorizationManagementService: AuthorizationManagementService,
  purposeManagementService: PurposeManagementService,
  tenantManagementService: TenantManagementService,
  fileManager: FileManager,
  pdfCreator: PDFCreator,
  uuidSupplier: UUIDSupplier,
  dateTimeSupplier: OffsetDateTimeSupplier
)(implicit ec: ExecutionContext) {

  private[this] val riskAnalysisTemplate = Source
    .fromResource("riskAnalysisTemplate/index.html")
    .getLines()
    .mkString(System.lineSeparator())

  def activateOrWaitForApproval(
    eService: EService,
    purpose: Purpose,
    version: PurposeVersion,
    organizationId: UUID,
    ownership: Ownership
  )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = {

    def changeToWaitForApproval(version: PurposeVersion, changedBy: ChangedBy): Future[PurposeVersion] =
      purposeManagementService.waitForApprovalPurposeVersion(
        purpose.id,
        version.id,
        StateChangeDetails(changedBy, dateTimeSupplier.get())
      )

    def createWaitForApproval(version: PurposeVersion): Future[PurposeVersion] = for {
      _                         <- Future.traverse(purpose.versions.find(_.state == WAITING_FOR_APPROVAL).toList) { v =>
        purposeManagementService.deletePurposeVersion(purpose.id, v.id)
      }
      draftVersion              <- purposeManagementService.createPurposeVersion(
        purpose.id,
        PurposeVersionSeed(version.dailyCalls, version.riskAnalysis)
      )
      waitingForApprovalVersion <- purposeManagementService.waitForApprovalPurposeVersion(
        purpose.id,
        draftVersion.id,
        StateChangeDetails(ChangedBy.CONSUMER, dateTimeSupplier.get())
      )
    } yield waitingForApprovalVersion

    def activate(version: PurposeVersion, changedBy: ChangedBy): Future[PurposeVersion] = {

      val payload: ActivatePurposeVersionPayload =
        ActivatePurposeVersionPayload(
          riskAnalysis = version.riskAnalysis,
          stateChangeDetails = StateChangeDetails(changedBy, dateTimeSupplier.get())
        )

      for {
        version <- purposeManagementService.activatePurposeVersion(purpose.id, version.id, payload)
        _       <- authorizationManagementService.updateStateOnClients(
          purposeId = purpose.id,
          versionId = version.id,
          state = if (version.state == ACTIVE) ClientComponentState.ACTIVE else ClientComponentState.INACTIVE
        )
      } yield version

    }

    (version.state, ownership) match {
      case (DRAFT, CONSUMER | SELF_CONSUMER) =>
        isLoadAllowed(eService, purpose, version).ifM(
          firstVersionActivation(
            organizationId,
            purpose,
            version,
            StateChangeDetails(ChangedBy.CONSUMER, dateTimeSupplier.get()),
            eService
          ),
          changeToWaitForApproval(version, ChangedBy.CONSUMER)
        )
      case (DRAFT, PRODUCER)                 => Future.failed(OrganizationIsNotTheConsumer(organizationId))

      case (WAITING_FOR_APPROVAL, CONSUMER) => Future.failed(OrganizationIsNotTheProducer(organizationId))
      case (WAITING_FOR_APPROVAL, PRODUCER | SELF_CONSUMER) =>
        firstVersionActivation(
          organizationId,
          purpose,
          version,
          StateChangeDetails(ChangedBy.PRODUCER, dateTimeSupplier.get()),
          eService
        )

      case (SUSPENDED, CONSUMER)
          if purpose.suspendedByConsumer.contains(true) && purpose.suspendedByProducer.contains(true) =>
        activate(version, ChangedBy.CONSUMER)
      case (SUSPENDED, CONSUMER) if purpose.suspendedByConsumer.contains(true) =>
        isLoadAllowed(eService, purpose, version).ifM(
          activate(version, ChangedBy.CONSUMER),
          createWaitForApproval(version)
        )
      case (SUSPENDED, SELF_CONSUMER)                                          =>
        isLoadAllowed(eService, purpose, version).ifM(
          activate(version, ChangedBy.PRODUCER),
          createWaitForApproval(version)
        )
      case (SUSPENDED, PRODUCER)                                               =>
        activate(version, ChangedBy.PRODUCER)

      case _ => Future.failed(OrganizationNotAllowed(organizationId))
    }
  }

  /** Calculate if the load of the Version to activate will exceed the maximum load allowed by the EService,
    * considering all Purposes already activated for the Agreement
    * @param contexts Request contexts
    * @param eService EService of the Agreement related to the Purpose
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @return True if it will exceed, False otherwise
    */
  def isLoadAllowed(eService: EService, purpose: Purpose, version: PurposeVersion)(implicit
    contexts: Seq[(String, String)]
  ): Future[Boolean] = {
    for {
      consumerPurposes <- purposeManagementService.getPurposes(
        eserviceId = Some(purpose.eserviceId),
        consumerId = Some(purpose.consumerId),
        states = Seq(ACTIVE)
      )

      allPurposes <- purposeManagementService.getPurposes(
        eserviceId = Some(purpose.eserviceId),
        consumerId = None,
        states = Seq(ACTIVE)
      )

      agreements <- agreementManagementService.getAgreements(
        eServiceId = purpose.eserviceId,
        consumerId = purpose.consumerId,
        OPERATIVE_AGREEMENT_STATES
      )
      agreement  <- agreements.headOption.toFuture(AgreementNotFound(eService.id.toString, purpose.consumerId.toString))

      consumerActiveVersions    = consumerPurposes.purposes.flatMap(_.versions.filter(_.state == ACTIVE))
      allPurposesActiveVersions = allPurposes.purposes.flatMap(_.versions.filter(_.state == ACTIVE))

      consumerLoadRequestsSum = consumerActiveVersions.map(_.dailyCalls).sum
      allPurposesRequestsSum  = allPurposesActiveVersions.map(_.dailyCalls).sum

      currentDescriptor <- eService.descriptors
        .find(_.id == agreement.descriptorId)
        .toFuture(DescriptorNotFound(eService.id.toString, agreement.descriptorId.toString))

      maxDailyCallsPerConsumer = currentDescriptor.dailyCallsPerConsumer
      maxDailyCallsTotal       = currentDescriptor.dailyCallsTotal

    } yield consumerLoadRequestsSum + version.dailyCalls <= maxDailyCallsPerConsumer && (allPurposesRequestsSum + version.dailyCalls <= maxDailyCallsTotal)

  }

  private def getTenantName(tenantId: UUID)(implicit contexts: Seq[(String, String)]): Future[String] = for {
    tenant <- tenantManagementService.getTenant(tenantId)
  } yield tenant.name

  private def getTenantKind(tenantId: UUID)(implicit contexts: Seq[(String, String)]): Future[TenantKind] = for {
    tenant <- tenantManagementService.getTenant(tenantId)
  } yield tenant.kind

  /** Activate a Version for the first time, meaning when the current status is Draft or Waiting for Approval.
    * The first activation generates also the risk analysis document.
    *
    * @param contexts Request contexts
    * @param requesterId The requester
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @param stateChangeDetails Details on the organization that is performing the action
    * @return The updated Version
    */
  def firstVersionActivation(
    requesterId: UUID,
    purpose: Purpose,
    version: PurposeVersion,
    stateChangeDetails: StateChangeDetails,
    eService: EService
  )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = {
    val documentId: UUID = uuidSupplier.get()
    for {
      (producerDescription, consumerDescription) <- getTenantName(eService.producerId).zip(
        getTenantName(purpose.consumerId)
      )
      eServiceInfo = EServiceInfo(
        name = eService.name,
        producerName = producerDescription,
        consumerName = consumerDescription
      )
      tenantKind <- getTenantKind(requesterId)
      path       <- createRiskAnalysisDocument(documentId, purpose, version, eServiceInfo)(tenantKind)
      payload = ActivatePurposeVersionPayload(
        riskAnalysis = Some(
          PurposeVersionDocument(
            id = documentId,
            contentType = MediaTypes.`application/pdf`.toString(),
            path = path,
            createdAt = dateTimeSupplier.get()
          )
        ),
        stateChangeDetails = stateChangeDetails
      )
      updatedVersion <- purposeManagementService.activatePurposeVersion(purpose.id, version.id, payload)
      _              <- authorizationManagementService
        .updateStateOnClients(
          purposeId = purpose.id,
          versionId = version.id,
          state = if (updatedVersion.state == ACTIVE) ClientComponentState.ACTIVE else ClientComponentState.INACTIVE
        )
    } yield updatedVersion
  }

  /** Creates and store the Risk Analysis document based on document template and Purpose and Version information
    * @param documentId Document unique ID
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @return The path of the new document
    */
  def createRiskAnalysisDocument(
    documentId: UUID,
    purpose: Purpose,
    version: PurposeVersion,
    eServiceInfo: EServiceInfo
  )(kind: TenantKind): Future[String] = {
    for {
      riskAnalysisForm <- purpose.riskAnalysisForm.toFuture(MissingRiskAnalysis(purpose.id, version.id))
      document         <- pdfCreator.createDocument(
        riskAnalysisTemplate,
        riskAnalysisForm,
        version.dailyCalls,
        eServiceInfo,
        LanguageIt // TODO Language should be a request parameter
      )(kind)
      fileInfo = FileInfo("riskAnalysisDocument", document.getName, MediaTypes.`application/pdf`)
      path <- fileManager.store(ApplicationConfiguration.storageContainer, ApplicationConfiguration.storagePath)(
        documentId.toString,
        (fileInfo, document)
      )
    } yield path
  }
}
