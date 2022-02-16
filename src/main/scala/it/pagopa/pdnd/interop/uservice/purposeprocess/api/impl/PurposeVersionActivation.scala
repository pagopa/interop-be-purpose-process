package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.server.directives.FileInfo
import cats.implicits._
import it.pagopa.pdnd.interop.commons.files.service.{FileManager, StorageFilePath}
import it.pagopa.pdnd.interop.commons.utils.TypeConversions._
import it.pagopa.pdnd.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.EService
import it.pagopa.pdnd.interop.uservice.keymanagement.client.model.ClientComponentState
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.ChangedBy._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.PurposeVersionState._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model._
import it.pagopa.pdnd.interop.uservice.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.InternalErrors.{
  UserIsNotTheConsumer,
  UserIsNotTheProducer,
  UserNotAllowed
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.pdnd.interop.uservice.purposeprocess.service._

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

final case class PurposeVersionActivation(
  agreementManagementService: AgreementManagementService,
  authorizationManagementService: AuthorizationManagementService,
  purposeManagementService: PurposeManagementService,
  fileManager: FileManager,
  pdfCreator: PDFCreator,
  uuidSupplier: UUIDSupplier,
  dateTimeSupplier: OffsetDateTimeSupplier
)(implicit ec: ExecutionContext) {

  private[this] val riskAnalysisTemplate = Source
    .fromResource("riskAnalysisTemplate.html")
    .getLines()
    .mkString(System.lineSeparator())

  /** Activate or Wait for Approval for the given version based on current status and requester relationships.
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
    * @param bearerToken Bearer token
    * @param eService EService of the Agreement related to the Purpose
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @param userType Indicates the relationship of user with the Consumer or the Producer
    * @param userId User ID
    * @return the updated Version
    */
  def activateOrWaitForApproval(bearerToken: String)(
    eService: EService,
    purpose: Purpose,
    version: PurposeVersion,
    userType: ChangedBy,
    userId: UUID
  ): Future[PurposeVersion] = {

    val changeDetails = StateChangeDetails(changedBy = userType)

    def waitForApproval(): Future[PurposeVersion] =
      purposeManagementService
        .waitForApprovalPurposeVersion(bearerToken)(purpose.id, version.id, changeDetails)
    def activate(): Future[PurposeVersion] = {
      val payload =
        ActivatePurposeVersionPayload(riskAnalysis = version.riskAnalysis, stateChangeDetails = changeDetails)

      for {
        version <- purposeManagementService
          .activatePurposeVersion(bearerToken)(purpose.id, version.id, payload)
        _ <- authorizationManagementService.updateStateOnClients(bearerToken)(
          purposeId = purpose.id,
          state = ClientComponentState.ACTIVE
        )
      } yield version

    }

    (version.state, userType) match {
      case (DRAFT, CONSUMER) =>
        isLoadAllowed(bearerToken)(eService, purpose, version)
          .ifM(firstVersionActivation(bearerToken)(purpose, version, changeDetails), waitForApproval())
      case (DRAFT, PRODUCER) =>
        Future.failed(UserIsNotTheConsumer(userId))

      case (WAITING_FOR_APPROVAL, CONSUMER) =>
        Future.failed(UserIsNotTheProducer(userId))
      case (WAITING_FOR_APPROVAL, PRODUCER) =>
        firstVersionActivation(bearerToken)(purpose, version, changeDetails)

      case (SUSPENDED, CONSUMER) =>
        isLoadAllowed(bearerToken)(eService, purpose, version).ifM(activate(), waitForApproval())
      case (SUSPENDED, PRODUCER) =>
        activate()

      case _ =>
        Future.failed(UserNotAllowed(userId))
    }
  }

  /** Calculate if the load of the Version to activate will exceed the maximum load allowed by the EService,
    * considering all Purposes already activated for the Agreement
    * @param bearerToken Bearer token
    * @param eService EService of the Agreement related to the Purpose
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @return True if it will exceed, False otherwise
    */
  def isLoadAllowed(
    bearerToken: String
  )(eService: EService, purpose: Purpose, version: PurposeVersion): Future[Boolean] = {
    for {
      purposes <- purposeManagementService.getPurposes(bearerToken)(
        eserviceId = Some(purpose.eserviceId),
        consumerId = Some(purpose.consumerId),
        states = Seq(ACTIVE)
      )
      activeVersions = purposes.purposes.flatMap(_.versions.filter(_.state == ACTIVE))
      agreements <- agreementManagementService.getAgreements(bearerToken)(
        eServiceId = purpose.eserviceId,
        consumerId = purpose.consumerId
      )
      agreement <- agreements.headOption.toFuture(AgreementNotFound(eService.id.toString, purpose.consumerId.toString))
      loadRequestsSum = activeVersions.map(_.dailyCalls).sum
      maxDailyCalls <- eService.descriptors
        .find(_.id == agreement.descriptorId)
        .map(_.dailyCallsMaxNumber)
        .toFuture(DescriptorNotFound(eService.id.toString, agreement.descriptorId.toString))
    } yield loadRequestsSum + version.dailyCalls <= maxDailyCalls

  }

  /** Activate a Version for the first time, meaning when the current status is Draft or Waiting for Approval.
    * The first activation generates also the risk analysis document.
    *
    * @param bearerToken Beare token
    * @param purpose Purpose of the Version
    * @param version Version to activate
    * @param stateChangeDetails Details on the user that is performing the action
    * @return The updated Version
    */
  def firstVersionActivation(
    bearerToken: String
  )(purpose: Purpose, version: PurposeVersion, stateChangeDetails: StateChangeDetails): Future[PurposeVersion] = {
    val documentId: UUID = uuidSupplier.get
    for {
      path <- createRiskAnalysisDocument(documentId, purpose, version)
      payload = ActivatePurposeVersionPayload(
        riskAnalysis = Some(
          PurposeVersionDocument(
            id = documentId,
            contentType = MediaTypes.`application/pdf`.toString(),
            path = path,
            createdAt = dateTimeSupplier.get
          )
        ),
        stateChangeDetails = stateChangeDetails
      )
      updatedVersion <- purposeManagementService.activatePurposeVersion(bearerToken)(purpose.id, version.id, payload)
      _ <- authorizationManagementService
        .updateStateOnClients(bearerToken)(purposeId = purpose.id, state = ClientComponentState.ACTIVE)
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
    version: PurposeVersion
  ): Future[StorageFilePath] = {
    for {
      document <- pdfCreator.createDocument(riskAnalysisTemplate, purpose.riskAnalysisForm, version.dailyCalls)
      fileInfo = FileInfo("riskAnalysisDocument", document.getName, MediaTypes.`application/pdf`)
      path <- fileManager.store(ApplicationConfiguration.storageContainer, ApplicationConfiguration.storagePath)(
        documentId,
        (fileInfo, document)
      )
    } yield path
  }
}
