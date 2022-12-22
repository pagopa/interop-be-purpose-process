package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.server.directives.FileInfo
import cats.implicits._
import it.pagopa.interop.authorizationmanagement.client.model.ClientComponentState
import it.pagopa.interop.catalogmanagement.client.model.EService
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposemanagement.client.model.ChangedBy
import it.pagopa.interop.purposemanagement.client.model.PurposeVersionState._
import it.pagopa.interop.purposemanagement.client.model._
import it.pagopa.interop.purposeprocess.api.impl.Ownership.{CONSUMER, PRODUCER, SELF_CONSUMER}
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.interop.purposeprocess.error.InternalErrors.{
  OrganizationIsNotTheConsumer,
  OrganizationIsNotTheProducer,
  OrganizationNotAllowed
}
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.{EServiceInfo, LanguageIt}
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

  /** Activate or Wait for Approval for the given version based on current status.
    *
    * Validations
    * <table>
    *   <tr><th>Version State</th><th>Requester is a</th><th>Load exceeded</th><th>Result</th></tr>
    *   <tr><td>DRAFT</td><td>CONSUMER</td><td>No</td><td>Activate</td></tr>
    *   <tr><td>DRAFT</td><td>CONSUMER</td><td>Yes</td><td>Wait for Approval</td></tr>
    *   <tr><td>DRAFT</td><td>PRODUCER</td><td>-</td><td>Error: Unauthorized</td></tr>
    *   <tr><td>WAITING_FOR_APPROVAL</td><td>CONSUMER</td><td>-</td><td>Error: Unauthorized</td></tr>
    *   <tr><td>WAITING_FOR_APPROVAL</td><td>PRODUCER</td><td>-</td><td>Activate</td></tr>
    *   <tr><td>SUSPENDED</td><td>CONSUMER</td><td>No</td><td>Activate</td></tr>
    *   <tr><td>SUSPENDED</td><td>CONSUMER</td><td>Yes</td><td>Wait for Approval</td></tr>
    *   <tr><td>SUSPENDED</td><td>PRODUCER</td><td>-</td><td>Activate</td></tr>
    *   <tr><td><i>other</i></td><td>-</td><td>-</td><td>Error: Unauthorized</td></tr>
    * </table>
    * @param contexts Request contexts
    * @param eService EService of the Agreement related to the Purpose
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @param organizationId Organization ID
    * @param ownership The rule assumed by the organization
    * @return the updated Version
    */
  def activateOrWaitForApproval(
    eService: EService,
    purpose: Purpose,
    version: PurposeVersion,
    organizationId: UUID,
    ownership: Ownership
  )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = {

    def waitForApproval(): Future[PurposeVersion] =
      purposeManagementService.waitForApprovalPurposeVersion(
        purpose.id,
        version.id,
        StateChangeDetails(ChangedBy.CONSUMER)
      )

    def activate(changedBy: ChangedBy): Future[PurposeVersion] = {
      val payload: ActivatePurposeVersionPayload =
        ActivatePurposeVersionPayload(
          riskAnalysis = version.riskAnalysis,
          stateChangeDetails = StateChangeDetails(changedBy)
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
          firstVersionActivation(purpose, version, StateChangeDetails(ChangedBy.CONSUMER), eService),
          waitForApproval()
        )
      case (DRAFT, PRODUCER)                 => Future.failed(OrganizationIsNotTheConsumer(organizationId))

      case (WAITING_FOR_APPROVAL, CONSUMER) => Future.failed(OrganizationIsNotTheProducer(organizationId))
      case (WAITING_FOR_APPROVAL, PRODUCER | SELF_CONSUMER) =>
        firstVersionActivation(purpose, version, StateChangeDetails(ChangedBy.PRODUCER), eService)

      case (SUSPENDED, CONSUMER)                 =>
        isLoadAllowed(eService, purpose, version).ifM(activate(ChangedBy.CONSUMER), waitForApproval())
      case (SUSPENDED, PRODUCER | SELF_CONSUMER) => activate(ChangedBy.PRODUCER)
      case _                                     => Future.failed(OrganizationNotAllowed(organizationId))
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
        consumerId = purpose.consumerId
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

  def getTenantName(tenantId: UUID)(implicit contexts: Seq[(String, String)]): Future[String] = for {
    tenant <- tenantManagementService.getTenant(tenantId)
  } yield tenant.name

  /** Activate a Version for the first time, meaning when the current status is Draft or Waiting for Approval.
    * The first activation generates also the risk analysis document.
    *
    * @param contexts Request contexts
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @param stateChangeDetails Details on the organization that is performing the action
    * @return The updated Version
    */
  def firstVersionActivation(
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
      path <- createRiskAnalysisDocument(documentId, purpose, version, eServiceInfo)
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
  ): Future[String] = {
    for {
      riskAnalysisForm <- purpose.riskAnalysisForm.toFuture(
        MissingRiskAnalysis(purpose.id.toString, version.id.toString)
      )
      document         <- pdfCreator.createDocument(
        riskAnalysisTemplate,
        riskAnalysisForm,
        version.dailyCalls,
        eServiceInfo,
        LanguageIt // TODO Language should be a request parameter
      )
      fileInfo = FileInfo("riskAnalysisDocument", document.getName, MediaTypes.`application/pdf`)
      path <- fileManager.store(ApplicationConfiguration.storageContainer, ApplicationConfiguration.storagePath)(
        documentId.toString,
        (fileInfo, document)
      )
    } yield path
  }
}
