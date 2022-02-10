package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.server.directives.FileInfo
import cats.implicits._
import it.pagopa.pdnd.interop.commons.files.service.{FileManager, StorageFilePath}
import it.pagopa.pdnd.interop.commons.utils.TypeConversions._
import it.pagopa.pdnd.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.EService
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
      purposeManagementService
        .activatePurposeVersion(bearerToken)(purpose.id, version.id, payload)
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
    } yield updatedVersion
  }

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
