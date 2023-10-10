package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.server.Directives.onComplete
import akka.http.scaladsl.server.Route
import cats.syntax.all._
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagementDependency}
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.jwt._
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.commons.utils.AkkaUtils.getOrganizationIdFutureUUID
import it.pagopa.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.purposeprocess.api.PurposeApiService
import it.pagopa.interop.purposeprocess.api.impl.ResponseHandlers._
import it.pagopa.interop.purposeprocess.api.Adapters._
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposemanagement.model.purpose.{
  Draft,
  PersistentPurpose,
  PersistentPurposeVersion,
  WaitingForApproval
}
import it.pagopa.interop.catalogmanagement.model.{CatalogItem, Deliver, Receive}
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind
import it.pagopa.interop.purposeprocess.service.AgreementManagementService.OPERATIVE_AGREEMENT_STATES
import it.pagopa.interop.commons.riskanalysis.service.RiskAnalysisService
import it.pagopa.interop.commons.riskanalysis.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.service._

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import java.time.OffsetDateTime

final case class PurposeApiServiceImpl(
  agreementManagementService: AgreementManagementService,
  authorizationManagementService: AuthorizationManagementService,
  catalogManagementService: CatalogManagementService,
  purposeManagementService: PurposeManagementService,
  tenantManagementService: TenantManagementService,
  fileManager: FileManager,
  pdfCreator: PDFCreator,
  uuidSupplier: UUIDSupplier,
  dateTimeSupplier: OffsetDateTimeSupplier
)(implicit ec: ExecutionContext, readModel: ReadModelService)
    extends PurposeApiService {

  private implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  private[this] val purposeVersionActivation = PurposeVersionActivation(
    agreementManagementService,
    authorizationManagementService,
    purposeManagementService,
    tenantManagementService,
    fileManager,
    pdfCreator,
    uuidSupplier,
    dateTimeSupplier
  )

  override def getRiskAnalysisDocument(purposeId: String, versionId: String, documentId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersionDocument: ToEntityMarshaller[PurposeVersionDocument],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, SUPPORT_ROLE) {
    val operationLabel = s"Retrieving Risk Analysis document $documentId for Purpose $purposeId and Version $versionId"
    logger.info(operationLabel)

    val result: Future[PurposeVersionDocument] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      documentUUID   <- documentId.toFutureUUID
      purpose        <- purposeManagementService.getPurposeById(purposeUUID)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      _              <- Ownership.getOrganizationRole(organizationId, eService.producerId, purpose.consumerId).toFuture
      version        <- getVersion(purpose, versionUUID)
      document       <- version.riskAnalysis
        .find(_.id == documentUUID)
        .toFuture(PurposeVersionDocumentNotFound(purposeId, versionId, documentId))
    } yield document.toApi

    onComplete(result) {
      getRiskAnalysisDocumentResponse[PurposeVersionDocument](operationLabel)(getRiskAnalysisDocument200)
    }
  }

  private def checkFreeOfCharge(isFreeOfCharge: Boolean, freeOfChargeReason: Option[String]): Future[Unit] =
    if (isFreeOfCharge && freeOfChargeReason.isEmpty) Future.failed(MissingFreeOfChargeReason)
    else Future.unit

  private def getTenantKind(requesterId: UUID): Future[PersistentTenantKind] = for {
    tenant     <- tenantManagementService.getTenantById(requesterId)
    tenantKind <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
  } yield tenantKind

  private def checkAgreements(eServiceId: UUID, consumerId: UUID, title: String): Future[Unit] = for {
    agreements   <- agreementManagementService.getAgreements(eServiceId, consumerId, OPERATIVE_AGREEMENT_STATES)
    agreement    <- agreements.headOption.toFuture(AgreementNotFound(eServiceId.toString, consumerId.toString))
    maybePurpose <- purposeManagementService
      .listPurposes(
        consumerId,
        title.some,
        List(agreement.eserviceId.toString),
        List(agreement.consumerId.toString),
        List(agreement.producerId.toString),
        states = List.empty,
        excludeDraft = false,
        offset = 0,
        limit = 1,
        exactMatchOnTitle = true
      )
      .map(_.results.headOption)

    _ <- maybePurpose.fold(Future.unit)(_ => Future.failed(DuplicatedPurposeName(title)))
  } yield ()

  override def createPurposeFromEService(seed: EServicePurposeSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel =
      s"Creating Purposes for EService ${seed.eServiceId}, Consumer ${seed.consumerId}"

    val result: Future[Purpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      _              <- assertOrganizationIsAConsumer(organizationId, seed.consumerId)
      eService       <- catalogManagementService.getEServiceById(seed.eServiceId)
      _ <- if (eService.mode == Receive) Future.unit else Future.failed(EServiceNotInReceiveMode(eService.id))
      riskAnalysis <- eService.riskAnalysis
        .find(_.id == seed.riskAnalysisId)
        .toFuture(RiskAnalysisNotFound(seed.eServiceId, seed.riskAnalysisId))
      _            <- checkFreeOfCharge(seed.isFreeOfCharge, seed.freeOfChargeReason)
      tenantKind   <- getTenantKind(organizationId)
      purposeSeed = seed.toManagement(seed.eServiceId, riskAnalysis.riskAnalysisForm.toManagement(seed.riskAnalysisId))
      _       <- checkAgreements(seed.eServiceId, seed.consumerId, seed.title)
      purpose <- purposeManagementService.createPurpose(purposeSeed)
      isValidRiskAnalysisForm = isRiskAnalysisFormValid(
        riskAnalysisForm = purpose.riskAnalysisForm.map(_.toApi),
        schemaOnlyValidation = false
      )(tenantKind)
    } yield purpose.toApi(isRiskAnalysisValid = isValidRiskAnalysisForm)

    onComplete(result) { createPurposeFromEServiceResponse[Purpose](operationLabel)(createPurposeFromEService200) }
  }

  override def createPurpose(seed: PurposeSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Creating Purpose for EService ${seed.eserviceId} and Consumer ${seed.consumerId}"
    logger.info(operationLabel)

    val result: Future[Purpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      _              <- assertOrganizationIsAConsumer(organizationId, seed.consumerId)
      _              <- checkFreeOfCharge(seed.isFreeOfCharge, seed.freeOfChargeReason)
      tenantKind     <- getTenantKind(organizationId)
      purposeSeed    <- seed.toManagement(schemaOnlyValidation = true)(tenantKind).toFuture
      _              <- checkAgreements(seed.eserviceId, seed.consumerId, seed.title)
      purpose        <- purposeManagementService.createPurpose(purposeSeed)
      isValidRiskAnalysisForm = isRiskAnalysisFormValid(
        riskAnalysisForm = purpose.riskAnalysisForm.map(_.toApi),
        schemaOnlyValidation = false
      )(tenantKind)

    } yield purpose.toApi(isRiskAnalysisValid = isValidRiskAnalysisForm)

    onComplete(result) { createPurposeResponse[Purpose](operationLabel)(createPurpose200) }
  }

  override def createPurposeVersion(purposeId: String, seed: PurposeVersionSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Creating Purpose Version for Purpose $purposeId"
    logger.info(operationLabel)

    def publish(
      organizationId: UUID,
      purpose: PersistentPurpose,
      version: PersistentPurposeVersion
    ): Future[PurposeManagementDependency.PurposeVersion] = for {
      eService  <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership <- Ownership
        .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
        .toFuture
      published <- purposeVersionActivation.activateOrWaitForApproval(
        eService,
        purpose,
        version,
        organizationId,
        ownership
      )
    } yield published

    val result: Future[PurposeVersion] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      purpose        <- purposeManagementService.getPurposeById(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      version        <- purposeManagementService.createPurposeVersion(purposeUUID, seed.toManagement)
      published      <- publish(organizationId, purpose, version.toPersistent)
    } yield published.toApi
    onComplete(result) { createPurposeVersionResponse[PurposeVersion](operationLabel)(createPurposeVersion200) }
  }

  override def updatePurpose(purposeId: String, seed: PurposeUpdateContent)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Updating Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[Purpose] = updatePurpose(
      purposeId,
      eService => if (eService.mode == Deliver) Future.unit else Future.failed(EServiceNotInDeliverMode(eService.id)),
      seed.isFreeOfCharge,
      seed.freeOfChargeReason,
      (_, tenantKind) =>
        seed
          .toManagement(schemaOnlyValidation = true)(tenantKind)
          .toFuture
    )

    onComplete(result) { updatePurposeResponse[Purpose](operationLabel)(updatePurpose200) }
  }

  override def updateReversePurpose(purposeId: String, seed: ReversePurposeUpdateContent)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Updating Reverse Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[Purpose] = updatePurpose(
      purposeId,
      eService => if (eService.mode == Receive) Future.unit else Future.failed(EServiceNotInReceiveMode(eService.id)),
      seed.isFreeOfCharge,
      seed.freeOfChargeReason,
      (purpose, tenantKind) =>
        seed
          .toManagement(schemaOnlyValidation = true, purpose.riskAnalysisForm.map(_.toApi))(tenantKind)
          .toFuture
    )

    onComplete(result) { updateReversePurposeResponse[Purpose](operationLabel)(updatePurpose200) }
  }

  private def updatePurpose(
    purposeId: String,
    eServiceModeCheck: CatalogItem => Future[Unit],
    isFreeOfCharge: Boolean,
    freeOfChargeReason: Option[String],
    payload: (PersistentPurpose, PersistentTenantKind) => Future[PurposeManagementDependency.PurposeUpdateContent]
  )(implicit contexts: Seq[(String, String)]): Future[Purpose] = for {
    organizationId <- getOrganizationIdFutureUUID(contexts)
    purposeUUID    <- purposeId.toFutureUUID
    purpose        <- purposeManagementService.getPurposeById(purposeUUID)
    _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
    _              <- assertPurposeIsInDraftState(purpose)
    eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
    _              <- eServiceModeCheck(eService)
    _              <- checkFreeOfCharge(isFreeOfCharge, freeOfChargeReason)
    tenant         <- tenantManagementService.getTenantById(organizationId)
    tenantKind     <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
    purposePayload <- payload(purpose, tenantKind)
    updatedPurpose <- purposeManagementService.updatePurpose(purposeUUID, purposePayload)
    isValidRiskAnalysisForm = isRiskAnalysisFormValid(updatedPurpose.riskAnalysisForm.map(_.toApi))(tenantKind)
  } yield updatedPurpose.toApi(isRiskAnalysisValid = isValidRiskAnalysisForm)

  override def getPurpose(id: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, API_ROLE, SECURITY_ROLE, M2M_ROLE, SUPPORT_ROLE) {
    val operationLabel = s"Retrieving Purpose $id"
    logger.info(operationLabel)

    def isDraft(purpose: PersistentPurpose): Boolean =
      purpose.versions.map(_.state) == Seq(Draft)

    def authorizeRiskAnalysisForm(
      purpose: PersistentPurpose,
      producerId: UUID,
      organizationId: UUID,
      tenantKind: PersistentTenantKind
    ): Purpose = {
      if (organizationId == purpose.consumerId || organizationId == producerId)
        if (isDraft(purpose))
          purpose.toApi(isRiskAnalysisFormValid(purpose.riskAnalysisForm.map(_.toApi))(tenantKind))
        else purpose.toApi(isRiskAnalysisValid = true)
      else
        purpose
          .copy(riskAnalysisForm = None)
          .toApi(isRiskAnalysisValid = false) // Hide risk analysis to other organizations
    }

    val result: Future[Purpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      uuid           <- id.toFutureUUID
      purpose        <- purposeManagementService.getPurposeById(uuid)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      tenant         <- tenantManagementService.getTenantById(organizationId)
      tenantKind     <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
      authorizedPurpose = authorizeRiskAnalysisForm(
        purpose,
        producerId = eService.producerId,
        organizationId = organizationId,
        tenantKind
      )
    } yield authorizedPurpose

    onComplete(result) { getPurposeResponse[Purpose](operationLabel)(getPurpose200) }
  }

  override def getPurposes(
    name: Option[String],
    eServicesIds: String,
    consumersIds: String,
    producersIds: String,
    states: String,
    excludeDraft: Boolean,
    offset: Int,
    limit: Int
  )(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposes: ToEntityMarshaller[Purposes],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, API_ROLE, SECURITY_ROLE, M2M_ROLE, INTERNAL_ROLE, SUPPORT_ROLE) {
    val operationLabel =
      s"Retrieving Purposes for name $name, EServices $eServicesIds, Consumers $consumersIds, Producers $producersIds"

    val result: Future[Purposes] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      states         <- parseArrayParameters(states).traverse(PurposeVersionState.fromValue).toFuture
      purposes       <- purposeManagementService.listPurposes(
        organizationId,
        name,
        parseArrayParameters(eServicesIds),
        parseArrayParameters(consumersIds),
        parseArrayParameters(producersIds),
        states.map(_.toPersistent),
        excludeDraft,
        offset,
        limit
      )
      apiPurposes = purposes.results.map(_.toApi(false)).map(_.copy(riskAnalysisForm = None))
    } yield Purposes(results = apiPurposes, totalCount = purposes.totalCount)

    onComplete(result) { getPurposesResponse[Purposes](operationLabel)(getPurposes200) }
  }

  override def deletePurpose(
    id: String
  )(implicit contexts: Seq[(String, String)], toEntityMarshallerProblem: ToEntityMarshaller[Problem]): Route =
    authorize(ADMIN_ROLE) {
      val operationLabel = s"Deleting Purpose $id"
      logger.info(operationLabel)

      def isDeletable(purpose: PersistentPurpose): Boolean = {
        val states = purpose.versions.map(_.state)
        states.isEmpty ||
        states == Seq(Draft) ||
        states == Seq(WaitingForApproval)
      }

      val result: Future[Unit] = for {
        organizationId <- getOrganizationIdFutureUUID(contexts)
        purposeUUID    <- id.toFutureUUID
        purpose        <- purposeManagementService.getPurposeById(purposeUUID)
        _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
        _              <- Future.successful(purpose).ensure(PurposeCannotBeDeleted(id))(isDeletable)
        clients        <- authorizationManagementService.getClients(purposeUUID)
        _              <- Future.traverse(clients)(client =>
          authorizationManagementService.removePurposeFromClient(purposeUUID, client.id)
        )
        _              <- Future.traverse(purpose.versions)(version =>
          purposeManagementService.deletePurposeVersion(purposeUUID, version.id)
        )
        _              <- purposeManagementService.deletePurpose(purposeUUID)
      } yield ()

      onComplete(result) { deletePurposeResponse[Unit](operationLabel)(_ => deletePurpose204) }
    }

  override def deletePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, INTERNAL_ROLE) {
    val operationLabel = s"Deleting Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    def assertIsDeletable(version: PersistentPurposeVersion): Future[Unit] =
      Future
        .failed(PurposeVersionCannotBeDeleted(purposeId, versionId))
        .unlessA(
          version.state == Draft ||
            version.state == WaitingForApproval
        )

    val result: Future[Unit] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      purpose        <- purposeManagementService.getPurposeById(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      version        <- getVersion(purpose, versionUUID)
      _              <- assertIsDeletable(version)
      _              <- purposeManagementService.deletePurposeVersion(purposeUUID, versionUUID)
    } yield ()

    onComplete(result) { deletePurposeVersionResponse[Unit](operationLabel)(_ => deletePurposeVersion204) }
  }

  override def activatePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Activating Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[PurposeVersion] = for {
      purposeUUID      <- purposeId.toFutureUUID
      versionUUID      <- versionId.toFutureUUID
      organizationId   <- getOrganizationIdFutureUUID(contexts)
      purpose          <- purposeManagementService.getPurposeById(purposeUUID)
      consumer         <- tenantManagementService.getTenantById(purpose.consumerId)
      tenantKind       <- consumer.kind.toFuture(TenantKindNotFound(consumer.id))
      version          <- getVersion(purpose, versionUUID)
      riskAnalysisForm <- purpose.riskAnalysisForm.toFuture(MissingRiskAnalysis(purposeUUID))
      _                <-
        RiskAnalysisValidation
          .validate(riskAnalysisForm.toApi.toTemplate, schemaOnlyValidation = false)(tenantKind.toTemplate)
          .leftMap(RiskAnalysisValidationFailed(_))
          .toEither
          .whenA(version.state == Draft)
          .toFuture
      eService         <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership        <- Ownership
        .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
        .toFuture
      updatedVersion   <- purposeVersionActivation.activateOrWaitForApproval(
        eService,
        purpose,
        version,
        organizationId,
        ownership
      )
    } yield updatedVersion.toApi

    onComplete(result) { activatePurposeVersionResponse[PurposeVersion](operationLabel)(activatePurposeVersion200) }
  }

  override def suspendPurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Suspending Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurposeById(purposeUUID)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership      <- Ownership
        .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
        .toFuture
      _              <- getVersion(purpose, versionUUID)
      stateDetails = PurposeManagementDependency.StateChangeDetails(ownership.toChangedBy, dateTimeSupplier.get())
      purposeVersion <- purposeManagementService.suspendPurposeVersion(purposeUUID, versionUUID, stateDetails)
      _              <- authorizationManagementService.updateStateOnClients(
        purposeId = purposeUUID,
        versionId = versionUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield purposeVersion.toApi

    onComplete(result) { suspendPurposeVersionResponse[PurposeVersion](operationLabel)(suspendPurposeVersion200) }
  }

  override def archivePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, INTERNAL_ROLE) {
    val operationLabel = s"Archiving Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurposeById(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      _              <- getVersion(purpose, versionUUID)
      stateDetails = PurposeManagementDependency.StateChangeDetails(
        PurposeManagementDependency.ChangedBy.CONSUMER,
        dateTimeSupplier.get()
      )
      _              <- Future.traverse(purpose.versions.find(_.state == WaitingForApproval).toList)(v =>
        purposeManagementService.deletePurposeVersion(purpose.id, v.id)
      )
      purposeVersion <- purposeManagementService.archivePurposeVersion(purposeUUID, versionUUID, stateDetails)
      _              <- authorizationManagementService.updateStateOnClients(
        purposeId = purposeUUID,
        versionId = versionUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield purposeVersion.toApi

    onComplete(result) { archivePurposeVersionResponse[PurposeVersion](operationLabel)(archivePurposeVersion200) }
  }

  override def updateWaitingForApprovalPurposeVersion(
    purposeId: String,
    versionId: String,
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  )(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Updating Waiting For Approval Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurposeById(purposeUUID)
      _              <- assertOrganizationIsAProducer(organizationId, purpose.eserviceId)
      _              <- getVersion(purpose, versionUUID)
      purposeVersion <- purposeManagementService.updateWaitingForApprovalPurposeVersion(
        purposeUUID,
        versionUUID,
        updateContent.toManagement
      )
    } yield purposeVersion.toApi

    onComplete(result) {
      updateWaitingForApprovalPurposeVersionResponse[PurposeVersion](operationLabel)(
        updateWaitingForApprovalPurposeVersion200
      )
    }
  }

  override def clonePurpose(purposeId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route =
    authorize(ADMIN_ROLE) {
      val operationLabel = s"Cloning Purpose $purposeId"
      logger.info(operationLabel)

      def isClonable(purpose: PersistentPurpose): Boolean = {
        val states = purpose.versions.map(_.state)

        states.nonEmpty &&
        states != Seq(Draft)
      }

      def createPurposeSeed(purpose: PersistentPurpose, dailyCalls: Int): PurposeSeed =
        PurposeSeed(
          eserviceId = purpose.eserviceId,
          consumerId = purpose.consumerId,
          riskAnalysisForm = purpose.riskAnalysisForm.map(_.toApi),
          title = s"${purpose.title} - clone",
          description = purpose.description,
          isFreeOfCharge = purpose.isFreeOfCharge,
          freeOfChargeReason = purpose.freeOfChargeReason,
          dailyCalls = dailyCalls
        )

      def getDailyCalls(versions: Seq[PersistentPurposeVersion]): Int = {

        val ordering: Ordering[OffsetDateTime] = Ordering(Ordering.by[OffsetDateTime, Long](_.toEpochSecond).reverse)

        val latestNoWaiting: Option[Int] = versions
          .filterNot(_.state == WaitingForApproval)
          .sortBy(_.createdAt)(ordering)
          .map(_.dailyCalls)
          .headOption

        val latestAll: Option[Int] = versions.sortBy(_.createdAt)(ordering).map(_.dailyCalls).headOption

        latestNoWaiting.getOrElse(latestAll.getOrElse(0))
      }

      val result: Future[Purpose] = for {
        organizationId <- getOrganizationIdFutureUUID(contexts)
        tenant         <- tenantManagementService.getTenantById(organizationId)
        purposeUUID    <- purposeId.toFutureUUID
        purpose        <- purposeManagementService.getPurposeById(purposeUUID)
        dailyCalls = getDailyCalls(purpose.versions)
        _ <-
          if (isClonable(purpose)) Future.successful(purpose)
          else Future.failed(PurposeCannotBeCloned(purposeId))
        dependencySeed = createPurposeSeed(purpose, dailyCalls)
        tenantKind  <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
        purposeSeed <- dependencySeed
          .toManagement(schemaOnlyValidation = true)(tenantKind)
          .toFuture
        newPurpose  <- purposeManagementService.createPurpose(purposeSeed)
        isValidRiskAnalysisForm = isRiskAnalysisFormValid(newPurpose.riskAnalysisForm.map(_.toApi))(tenantKind)
      } yield newPurpose.toApi(isRiskAnalysisValid = isValidRiskAnalysisForm)

      onComplete(result) { clonePurposeResponse[Purpose](operationLabel)(clonePurpose200) }
    }

  override def retrieveLatestRiskAnalysisConfiguration(tenantKind: Option[String])(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerRiskAnalysisFormConfigResponse: ToEntityMarshaller[RiskAnalysisFormConfigResponse],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, SUPPORT_ROLE) {
    val operationLabel = s"Retrieve latest risk analysis configuration"
    logger.info(operationLabel)

    val result: Future[RiskAnalysisFormConfigResponse] = for {
      organizationId  <- getOrganizationIdFutureUUID(contexts)
      tenant          <- tenantManagementService.getTenantById(organizationId)
      tenantKindParam <- tenantKind.traverse(TenantKind.fromValue).toFuture
      kind            <- tenantKindParam.fold(tenant.kind.toFuture(TenantKindNotFound(tenant.id)))(kind =>
        Future.successful(kind.toPersistent)
      )
      kindConfig      <- RiskAnalysisService
        .riskAnalysisForms()
        .get(kind.toTemplate)
        .toFuture(RiskAnalysisConfigForTenantKindNotFound(tenant.id))
      (latest, riskAnalysisFormConfig) <- kindConfig
        .maxByOption(_._1.toDouble)
        .toFuture(RiskAnalysisConfigLatestVersionNotFound(kind))
    } yield riskAnalysisFormConfig.toApi

    onComplete(result) {
      retrieveLatestRiskAnalysisConfigurationResponse[RiskAnalysisFormConfigResponse](operationLabel)(
        retrieveLatestRiskAnalysisConfiguration200
      )
    }
  }

  override def retrieveRiskAnalysisConfigurationByVersion(tenantKind: Option[String], riskAnalysisVersion: String)(
    implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerRiskAnalysisFormConfigResponse: ToEntityMarshaller[RiskAnalysisFormConfigResponse],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, SUPPORT_ROLE) {
    val operationLabel = s"Retrieve version $riskAnalysisVersion of risk analysis configuration"
    logger.info(operationLabel)

    val result: Future[RiskAnalysisFormConfigResponse] = for {
      organizationId         <- getOrganizationIdFutureUUID(contexts)
      tenant                 <- tenantManagementService.getTenantById(organizationId)
      tenantKindParam        <- tenantKind.traverse(TenantKind.fromValue).toFuture
      kind                   <- tenantKindParam.fold(tenant.kind.toFuture(TenantKindNotFound(tenant.id)))(kind =>
        Future.successful(kind.toPersistent)
      )
      kindConfig             <- RiskAnalysisService
        .riskAnalysisForms()
        .get(kind.toTemplate)
        .toFuture(RiskAnalysisConfigForTenantKindNotFound(tenant.id))
      riskAnalysisFormConfig <- kindConfig
        .get(riskAnalysisVersion)
        .toFuture(RiskAnalysisConfigVersionNotFound(riskAnalysisVersion, kind))
    } yield riskAnalysisFormConfig.toApi

    onComplete(result) {
      retrieveRiskAnalysisConfigurationByVersionResponse[RiskAnalysisFormConfigResponse](operationLabel)(
        retrieveRiskAnalysisConfigurationByVersion200
      )
    }
  }

  private def assertOrganizationIsAConsumer(organizationId: UUID, consumerId: UUID): Future[Ownership] =
    if (organizationId == consumerId) Future.successful(Ownership.CONSUMER)
    else Future.failed(OrganizationIsNotTheConsumer(organizationId))

  private def assertOrganizationIsAProducer(organizationId: UUID, eServiceId: UUID): Future[Ownership] =
    for {
      eService <- catalogManagementService.getEServiceById(eServiceId)
      _ <- Future.failed(OrganizationIsNotTheProducer(organizationId)).unlessA(organizationId == eService.producerId)
    } yield Ownership.PRODUCER

  private def getVersion(purpose: PersistentPurpose, versionId: UUID) =
    purpose.versions.find(_.id == versionId).toFuture(PurposeVersionNotFound(purpose.id, versionId))

  private def assertPurposeIsInDraftState(purpose: PersistentPurpose): Future[Unit] = {
    if (purpose.versions.map(_.state) == Seq(Draft))
      Future.successful(())
    else Future.failed(PurposeNotInDraftState(purpose.id))
  }

  private def isRiskAnalysisFormValid(
    riskAnalysisForm: Option[RiskAnalysisForm],
    schemaOnlyValidation: Boolean = false
  )(kind: PersistentTenantKind): Boolean =
    riskAnalysisForm.exists(risk =>
      RiskAnalysisValidation
        .validate(risk.toTemplate, schemaOnlyValidation)(kind.toTemplate)
        .isValid
    )

}
