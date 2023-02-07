package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.{ContentType, HttpEntity, MediaTypes}
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.Route
import cats.implicits._
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagementDependency}
import it.pagopa.interop.catalogmanagement.client.{model => CatalogManagementDependency}
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.jwt.{ADMIN_ROLE, M2M_ROLE, API_ROLE, SECURITY_ROLE, authorize}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.commons.utils.AkkaUtils.getOrganizationIdFutureUUID
import it.pagopa.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.purposeprocess.api.PurposeApiService
import it.pagopa.interop.purposeprocess.api.converters.agreementmanagement.AgreementConverter
import it.pagopa.interop.purposeprocess.api.converters.authorizationmanagement.ClientConverter
import it.pagopa.interop.purposeprocess.api.converters.catalogmanagement.EServiceConverter
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement._
import it.pagopa.interop.purposeprocess.api.converters.tenantmanagement.OrganizationConverter
import it.pagopa.interop.purposeprocess.api.impl.ResponseHandlers._
import it.pagopa.interop.purposeprocess.common.readmodel.ReadModelQueries
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposeprocess.service.AgreementManagementService.OPERATIVE_AGREEMENT_STATES
import it.pagopa.interop.purposeprocess.service._

import java.io.File
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

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
    toEntityMarshallerProblem: ToEntityMarshaller[Problem],
    toEntityMarshallerFile: ToEntityMarshaller[File]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Retrieving Risk Analysis document $documentId for Purpose $purposeId and Version $versionId"
    logger.info(operationLabel)

    val result: Future[HttpEntity.Strict] = for {
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
      byteStream     <- fileManager.get(ApplicationConfiguration.storageContainer)(document.path)
    } yield HttpEntity(ContentType(MediaTypes.`application/pdf`), byteStream.toByteArray)

    onComplete(result) { getRiskAnalysisDocumentResponse[HttpEntity.Strict](operationLabel)(complete(_)) }
  }

  override def createPurpose(seed: PurposeSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[OldPurpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Creating Purpose for EService ${seed.eserviceId} and Consumer ${seed.consumerId}"
    logger.info(operationLabel)

    val result: Future[OldPurpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      ownership      <- assertOrganizationIsAConsumer(organizationId, seed.consumerId)
      clientSeed     <- PurposeSeedConverter.apiToDependency(seed).toFuture
      agreements     <- agreementManagementService.getAgreements(
        seed.eserviceId,
        seed.consumerId,
        OPERATIVE_AGREEMENT_STATES
      )
      _        <- agreements.headOption.toFuture(AgreementNotFound(seed.eserviceId.toString, seed.consumerId.toString))
      purpose  <- purposeManagementService.createPurpose(clientSeed)
      eService <- catalogManagementService.getEServiceById(purpose.eserviceId)
      result   <- enhancePurpose(purpose, eService, ownership)
    } yield result

    onComplete(result) { createPurposeResponse[OldPurpose](operationLabel)(createPurpose201) }
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
    toEntityMarshallerPurpose: ToEntityMarshaller[OldPurpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    val operationLabel = s"Updating Purpose $purposeId"
    logger.info(operationLabel)

    val result: Future[OldPurpose] = for {
      organizationId  <- getOrganizationIdFutureUUID(contexts)
      purposeUUID     <- purposeId.toFutureUUID
      purpose         <- purposeManagementService.getPurpose(purposeUUID)
      ownership       <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      depPayload      <- PurposeUpdateContentConverter.apiToDependency(purposeUpdateContent).toFuture
      updatedPurpose  <- purposeManagementService.updatePurpose(purposeUUID, depPayload)
      eService        <- catalogManagementService.getEServiceById(updatedPurpose.eserviceId)
      enhancedPurpose <- enhancePurpose(updatedPurpose, eService, ownership)
    } yield enhancedPurpose

    onComplete(result) { updatePurposeResponse[OldPurpose](operationLabel)(updatePurpose200) }
  }

  override def getPurpose(id: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[OldPurpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, API_ROLE, SECURITY_ROLE, M2M_ROLE) {
    val operationLabel = s"Retrieving Purpose $id"
    logger.info(operationLabel)

    val result: Future[OldPurpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      uuid           <- id.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(uuid)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership      <- Ownership.getOrganizationRole(organizationId, eService.producerId, purpose.consumerId).toFuture
      result         <- enhancePurpose(purpose, eService, ownership)
    } yield result

    onComplete(result) { getPurposeResponse[OldPurpose](operationLabel)(getPurpose200) }
  }

  override def getPurposes(
    name: Option[String],
    eServicesIds: String,
    consumersIds: String,
    producersIds: String,
    states: String,
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
        offset,
        limit
      )(readModel)
      apiPurposes = purposes.results.map(PurposeConverter.persistentToApi)
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
      stateDetails = PurposeManagementDependency.StateChangeDetails(ownership.toChangedBy)
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
      stateDetails = PurposeManagementDependency.StateChangeDetails(PurposeManagementDependency.ChangedBy.CONSUMER)
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
      _              <- getVersion(purpose, versionUUID)
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

  private def enhancePurpose(
    depPurpose: PurposeManagementDependency.Purpose,
    eService: CatalogManagementDependency.EService,
    ownership: Ownership
  )(implicit contexts: Seq[(String, String)]): Future[OldPurpose] = {
    def clientsByUserType(): Future[List[Client]] =
      ownership match {
        case Ownership.PRODUCER                           => List.empty[Client].pure[Future]
        case Ownership.CONSUMER | Ownership.SELF_CONSUMER =>
          authorizationManagementService
            .getClients(purposeId = depPurpose.id.some)
            .map(_.toList.map(ClientConverter.dependencyToApi))
      }

    for {
      depAgreements <- agreementManagementService.getAgreements(depPurpose.eserviceId, depPurpose.consumerId, Nil)
      depAgreement  <- depAgreements
        .sortBy(_.createdAt)
        .lastOption
        .toFuture(AgreementNotFound(depPurpose.eserviceId.toString, depPurpose.consumerId.toString))
      depProducer   <- tenantManagementService.getTenant(depAgreement.producerId)
      depConsumer   <- tenantManagementService.getTenant(depAgreement.consumerId)
      agreement = AgreementConverter.dependencyToApi(depAgreement)
      producer  = OrganizationConverter.dependencyToApi(eService.producerId, depProducer)
      consumer  = OrganizationConverter.dependencyToApi(depPurpose.consumerId, depConsumer)
      eService <- EServiceConverter.dependencyToApi(eService, depAgreement.descriptorId, producer).toFuture
      clients  <- clientsByUserType()
    } yield PurposeConverter
      .dependencyToOldApi(
        purpose = depPurpose,
        eService = eService,
        agreement = agreement,
        consumer = consumer,
        clients = clients
      )
  }

  private def getVersion(purpose: PurposeManagementDependency.Purpose, versionId: UUID) =
    purpose.versions.find(_.id == versionId).toFuture(PurposeVersionNotFound(purpose.id, versionId))

}
