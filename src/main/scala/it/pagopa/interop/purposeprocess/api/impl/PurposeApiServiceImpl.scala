package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.server.Directives.onComplete
import akka.http.scaladsl.server.Route
import cats.implicits._
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagementDependency}
import it.pagopa.interop.tenantmanagement.client.{model => TenantManagementDependency}
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
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement._
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.PurposeSeedConverter._
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.RiskAnalysisConverter._
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.PurposeUpdateContentConverter._
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.PurposeConverter._
import it.pagopa.interop.purposeprocess.api.impl.ResponseHandlers._
import it.pagopa.interop.purposeprocess.common.readmodel.ReadModelQueries
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposeprocess.service.AgreementManagementService.OPERATIVE_AGREEMENT_STATES
import it.pagopa.interop.purposeprocess.service.RiskAnalysisService
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
  readModel: ReadModelService,
  fileManager: FileManager,
  pdfCreator: PDFCreator,
  uuidSupplier: UUIDSupplier,
  dateTimeSupplier: OffsetDateTimeSupplier
)(implicit ec: ExecutionContext)
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
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Retrieving Risk Analysis document $documentId for Purpose $purposeId and Version $versionId"
    logger.info(operationLabel)

    val result: Future[PurposeVersionDocument] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      documentUUID   <- documentId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      _              <- Ownership.getOrganizationRole(organizationId, eService.producerId, purpose.consumerId).toFuture
      version        <- getVersion(purpose, versionUUID)
      document       <- version.riskAnalysis
        .find(_.id == documentUUID)
        .toFuture(PurposeVersionDocumentNotFound(purposeId, versionId, documentId))
    } yield PurposeVersionDocumentConverter.dependencyToApi(document)

    onComplete(result) {
      getRiskAnalysisDocumentResponse[PurposeVersionDocument](operationLabel)(getRiskAnalysisDocument200)
    }
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
      tenant         <- tenantManagementService.getTenant(organizationId)
      tenantKind     <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
      clientSeed     <- seed.apiToDependency(schemaOnlyValidation = true)(tenantKind).toFuture
      agreements     <- agreementManagementService.getAgreements(
        seed.eserviceId,
        seed.consumerId,
        OPERATIVE_AGREEMENT_STATES
      )
      agreement <- agreements.headOption.toFuture(AgreementNotFound(seed.eserviceId.toString, seed.consumerId.toString))
      maybePurpose <- ReadModelQueries
        .listPurposes(
          organizationId,
          seed.title.some,
          List(agreement.eserviceId.toString),
          List(agreement.consumerId.toString),
          List(agreement.producerId.toString),
          List.empty,
          false,
          0,
          1,
          exactMatchOnTitle = true
        )(readModel)
        .map(_.results.headOption)
      _            <- maybePurpose.fold(Future.unit)(_ => Future.failed(DuplicatedPurposeName(seed.title)))
      purpose      <- purposeManagementService.createPurpose(clientSeed)
      isValidRiskAnalysisForm = isRiskAnalysisFormValid(purpose.riskAnalysisForm)(tenantKind)

    } yield purpose.dependencyToApi(isRiskAnalysisValid = isValidRiskAnalysisForm)

    onComplete(result) { createPurposeResponse[Purpose](operationLabel)(createPurpose201) }
  }

  override def createPurposeVersion(purposeId: String, seed: PurposeVersionSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Creating Purpose Version for Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[PurposeVersion] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      depSeed = PurposeVersionSeedConverter.apiToDependency(seed)
      version <- purposeManagementService.createPurposeVersion(purposeUUID, depSeed)
    } yield PurposeVersionConverter.dependencyToApi(version)
    onComplete(result) { createPurposeVersionResponse[PurposeVersion](operationLabel)(createPurposeVersion201) }
  }

  override def updatePurpose(purposeId: String, purposeUpdateContent: PurposeUpdateContent)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Updating Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[Purpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      tenant         <- tenantManagementService.getTenant(organizationId)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      _              <- assertPurposeIsInDraftState(purpose)
      tenantKind     <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
      depPayload     <- purposeUpdateContent
        .apiToDependency(schemaOnlyValidation = true)(tenantKind)
        .toFuture
      updatedPurpose <- purposeManagementService.updatePurpose(purposeUUID, depPayload)
      isValidRiskAnalysisForm = isRiskAnalysisFormValid(purpose.riskAnalysisForm)(tenantKind)
    } yield updatedPurpose.dependencyToApi(isRiskAnalysisValid = isValidRiskAnalysisForm)

    onComplete(result) { updatePurposeResponse[Purpose](operationLabel)(updatePurpose200) }
  }

  override def getPurpose(id: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, API_ROLE, SECURITY_ROLE, M2M_ROLE) {
    val operationLabel = s"Retrieving Purpose $id"
    logger.info(operationLabel)

    def isDraft(purpose: PurposeManagementDependency.Purpose): Boolean =
      purpose.versions.map(_.state) == Seq(PurposeManagementDependency.PurposeVersionState.DRAFT)

    def authorizeRiskAnalysisForm(
      purpose: PurposeManagementDependency.Purpose,
      producerId: UUID,
      organizationId: UUID,
      tenantKind: TenantManagementDependency.TenantKind
    ): Purpose = {
      if (organizationId == purpose.consumerId || organizationId == producerId)
        if (isDraft(purpose)) purpose.dependencyToApi(isRiskAnalysisFormValid(purpose.riskAnalysisForm)(tenantKind))
        else purpose.dependencyToApi(isRiskAnalysisValid = true)
      else
        purpose
          .copy(riskAnalysisForm = None)
          .dependencyToApi(isRiskAnalysisValid = false) // Hide risk analysis to other organizations
    }

    val result: Future[Purpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      uuid           <- id.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(uuid)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      tenant         <- tenantManagementService.getTenant(organizationId)
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
  ): Route = authorize(ADMIN_ROLE, API_ROLE, SECURITY_ROLE, M2M_ROLE) {
    val operationLabel =
      s"Retrieving Purposes for name $name, EServices $eServicesIds, Consumers $consumersIds, Producers $producersIds"

    val result: Future[Purposes] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      statesEnum     <- parseArrayParameters(states).traverse(PurposeVersionState.fromValue).toFuture
      purposes       <- ReadModelQueries.listPurposes(
        organizationId,
        name,
        parseArrayParameters(eServicesIds),
        parseArrayParameters(consumersIds),
        parseArrayParameters(producersIds),
        statesEnum,
        excludeDraft,
        offset,
        limit
      )(readModel)
      apiPurposes = purposes.results.map(_.persistentToApi)
    } yield Purposes(results = apiPurposes, totalCount = purposes.totalCount)

    onComplete(result) { getPurposesResponse[Purposes](operationLabel)(getPurposes200) }
  }

  override def deletePurpose(
    id: String
  )(implicit contexts: Seq[(String, String)], toEntityMarshallerProblem: ToEntityMarshaller[Problem]): Route =
    authorize(ADMIN_ROLE) {
      val operationLabel = s"Deleting Purpose $id"
      logger.info(operationLabel)

      def isDeletable(purpose: PurposeManagementDependency.Purpose): Boolean = {
        val states = purpose.versions.map(_.state)
        states.isEmpty ||
        states == Seq(PurposeManagementDependency.PurposeVersionState.DRAFT) ||
        states == Seq(PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL)
      }

      val result: Future[Unit] = for {
        organizationId <- getOrganizationIdFutureUUID(contexts)
        purposeUUID    <- id.toFutureUUID
        purpose        <- purposeManagementService.getPurpose(purposeUUID)
        _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
        _              <- Future.successful(purpose).ensure(PurposeCannotBeDeleted(id))(isDeletable)
        clients        <- authorizationManagementService.getClients(Some(purposeUUID))
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
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Deleting Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    def assertIsDeletable(version: PurposeManagementDependency.PurposeVersion): Future[Unit] =
      Future
        .failed(PurposeVersionCannotBeDeleted(purposeId, versionId))
        .unlessA(
          version.state == PurposeManagementDependency.PurposeVersionState.DRAFT ||
            version.state == PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL
        )

    val result: Future[Unit] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
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
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      tenant         <- tenantManagementService.getTenant(organizationId)
      tenantKind     <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
      riskAnalysisForm = purpose.riskAnalysisForm.map(RiskAnalysisConverter.dependencyToApi)
      _              <- riskAnalysisForm
        .traverse(RiskAnalysisValidation.validate(_, schemaOnlyValidation = false)(tenantKind))
        .leftMap(RiskAnalysisValidationFailed(_))
        .toEither
        .toFuture
      version        <- getVersion(purpose, versionUUID)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership      <- Ownership
        .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
        .toFuture
      updatedVersion <- purposeVersionActivation.activateOrWaitForApproval(
        eService,
        purpose,
        version,
        organizationId,
        ownership
      )
    } yield PurposeVersionConverter.dependencyToApi(updatedVersion)

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
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership      <- Ownership
        .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
        .toFuture
      _              <- getVersion(purpose, versionUUID)
      stateDetails = PurposeManagementDependency.StateChangeDetails(ownership.toChangedBy, dateTimeSupplier.get())
      response <- purposeManagementService.suspendPurposeVersion(purposeUUID, versionUUID, stateDetails)
      _        <- authorizationManagementService.updateStateOnClients(
        purposeId = purposeUUID,
        versionId = versionUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    onComplete(result) { suspendPurposeVersionResponse[PurposeVersion](operationLabel)(suspendPurposeVersion200) }
  }

  override def archivePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Archiving Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      _              <- getVersion(purpose, versionUUID)
      stateDetails = PurposeManagementDependency.StateChangeDetails(
        PurposeManagementDependency.ChangedBy.CONSUMER,
        dateTimeSupplier.get()
      )
      _        <- Future.traverse(
        purpose.versions.find(_.state == PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL).toList
      )(v => purposeManagementService.deletePurposeVersion(purpose.id, v.id))
      response <- purposeManagementService.archivePurposeVersion(purposeUUID, versionUUID, stateDetails)
      _        <- authorizationManagementService.updateStateOnClients(
        purposeId = purposeUUID,
        versionId = versionUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    onComplete(result) { archivePurposeVersionResponse[PurposeVersion](operationLabel)(archivePurposeVersion200) }
  }

  override def updateDraftPurposeVersion(
    purposeId: String,
    versionId: String,
    updateContent: DraftPurposeVersionUpdateContent
  )(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Updating Draft Version $versionId of Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      purposeVersion <- getVersion(purpose, versionUUID)
      _              <- assertPurposeVersionIsInDraftState(purpose.id, purposeVersion)
      update = DraftPurposeVersionUpdateContentConverter.apiToDependency(updateContent)
      response <- purposeManagementService.updateDraftPurposeVersion(purposeUUID, versionUUID, update)
    } yield PurposeVersionConverter.dependencyToApi(response)

    onComplete(result) {
      updateDraftPurposeVersionResponse[PurposeVersion](operationLabel)(updateDraftPurposeVersion200)
    }

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
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAProducer(organizationId, purpose.eserviceId)
      _              <- getVersion(purpose, versionUUID)
      update = WaitingForApprovalPurposeVersionUpdateContentConverter.apiToDependency(updateContent)
      response <- purposeManagementService.updateWaitingForApprovalPurposeVersion(purposeUUID, versionUUID, update)
    } yield PurposeVersionConverter.dependencyToApi(response)

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

      def isClonable(purpose: PurposeManagementDependency.Purpose): Boolean = {
        val states = purpose.versions.map(_.state)

        states.nonEmpty &&
        states != Seq(PurposeManagementDependency.PurposeVersionState.DRAFT)
      }

      def createPurposeSeed(purpose: PurposeManagementDependency.Purpose): PurposeSeed =
        PurposeSeed(
          eserviceId = purpose.eserviceId,
          consumerId = purpose.consumerId,
          riskAnalysisForm = purpose.riskAnalysisForm.map(RiskAnalysisConverter.dependencyToApi),
          title = s"${purpose.title} - clone",
          description = purpose.description
        )

      def getDailyCalls(versions: Seq[PurposeManagementDependency.PurposeVersion]): Int = {

        val ordering: Ordering[OffsetDateTime] = Ordering(Ordering.by[OffsetDateTime, Long](_.toEpochSecond).reverse)

        val latestNoWaiting: Option[Int] = versions
          .filterNot(_.state == PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL)
          .sortBy(_.createdAt)(ordering)
          .map(_.dailyCalls)
          .headOption

        val latestAll: Option[Int] = versions.sortBy(_.createdAt)(ordering).map(_.dailyCalls).headOption

        latestNoWaiting.getOrElse(latestAll.getOrElse(0))
      }

      val result: Future[Purpose] = for {
        organizationId <- getOrganizationIdFutureUUID(contexts)
        tenant         <- tenantManagementService.getTenant(organizationId)
        purposeUUID    <- purposeId.toFutureUUID
        purpose        <- purposeManagementService.getPurpose(purposeUUID)
        _              <- Future.successful(purpose).ensure(PurposeCannotBeCloned(purposeId))(isClonable)
        dependencySeed = createPurposeSeed(purpose)
        tenantKind     <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
        apiPurposeSeed <- dependencySeed
          .apiToDependency(schemaOnlyValidation = true)(tenantKind)
          .toFuture
        newPurpose     <- purposeManagementService.createPurpose(apiPurposeSeed)
        dailyCalls            = getDailyCalls(purpose.versions)
        dependencyVersionSeed = PurposeVersionSeed(dailyCalls)
        apiVersionSeed        = PurposeVersionSeedConverter.apiToDependency(dependencyVersionSeed)
        _              <- purposeManagementService.createPurposeVersion(newPurpose.id, apiVersionSeed)
        updatedPurpose <- purposeManagementService.getPurpose(newPurpose.id)
        isValidRiskAnalysisForm = isRiskAnalysisFormValid(purpose.riskAnalysisForm)(tenantKind)
      } yield updatedPurpose.dependencyToApi(isRiskAnalysisValid = isValidRiskAnalysisForm)

      onComplete(result) { clonePurposeResponse[Purpose](operationLabel)(clonePurpose200) }
    }

  override def retrieveLatestRiskAnalysisConfiguration()(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerRiskAnalysisFormConfigResponse: ToEntityMarshaller[RiskAnalysisFormConfigResponse],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Retrieve latest risk analysis configuration"
    logger.info(operationLabel)

    val result: Future[RiskAnalysisFormConfigResponse] = for {
      organizationId                   <- getOrganizationIdFutureUUID(contexts)
      tenant                           <- tenantManagementService.getTenant(organizationId)
      kind                             <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
      kindConfig                       <- RiskAnalysisService
        .riskAnalysisForms()
        .get(kind)
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

  override def retrieveRiskAnalysisConfigurationByVersion(riskAnalysisVersion: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerRiskAnalysisFormConfigResponse: ToEntityMarshaller[RiskAnalysisFormConfigResponse],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Retrieve version $riskAnalysisVersion of risk analysis configuration"
    logger.info(operationLabel)

    val result: Future[RiskAnalysisFormConfigResponse] = for {
      organizationId         <- getOrganizationIdFutureUUID(contexts)
      tenant                 <- tenantManagementService.getTenant(organizationId)
      kind                   <- tenant.kind.toFuture(TenantKindNotFound(tenant.id))
      kindConfig             <- RiskAnalysisService
        .riskAnalysisForms()
        .get(kind)
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

  private def assertOrganizationIsAProducer(organizationId: UUID, eServiceId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Ownership] =
    for {
      eService <- catalogManagementService.getEServiceById(eServiceId)
      _ <- Future.failed(OrganizationIsNotTheProducer(organizationId)).unlessA(organizationId == eService.producerId)
    } yield Ownership.PRODUCER

  private def getVersion(purpose: PurposeManagementDependency.Purpose, versionId: UUID) =
    purpose.versions.find(_.id == versionId).toFuture(PurposeVersionNotFound(purpose.id, versionId))

  private def assertPurposeIsInDraftState(purpose: PurposeManagementDependency.Purpose): Future[Unit] = {
    if (purpose.versions.map(_.state) == Seq(PurposeManagementDependency.PurposeVersionState.DRAFT))
      Future.successful(())
    else Future.failed(PurposeNotInDraftState(purpose.id))
  }

  private def assertPurposeVersionIsInDraftState(
    purposeId: UUID,
    purposeVersion: PurposeManagementDependency.PurposeVersion
  ): Future[Unit] = {
    if (purposeVersion.state == PurposeManagementDependency.PurposeVersionState.DRAFT)
      Future.successful(())
    else Future.failed(PurposeVersionNotInDraftState(purposeId, purposeVersion.id))
  }

  private def isRiskAnalysisFormValid(
    riskAnalysisForm: Option[PurposeManagementDependency.RiskAnalysisForm]
  )(kind: TenantManagementDependency.TenantKind): Boolean =
    riskAnalysisForm
      .map(RiskAnalysisConverter.dependencyToApi(_))
      .map(
        RiskAnalysisValidation
          .validate(_, schemaOnlyValidation = false)(kind)
          .isValid
      )
      .getOrElse(false)

}
