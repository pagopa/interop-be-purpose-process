package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.{ContentType, HttpEntity, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.{Route, StandardRoute}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import it.pagopa.interop.agreementmanagement.client.invoker.{ApiError => AgreementApiError}
import it.pagopa.interop.authorizationmanagement.client.invoker.{ApiError => AuthorizationApiError}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagementDependency}
import it.pagopa.interop.catalogmanagement.client.invoker.{ApiError => CatalogApiError}
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.jwt.{ADMIN_ROLE, M2M_ROLE, authorizeInterop, hasPermissions}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.commons.utils.AkkaUtils.getOrganizationIdFutureUUID
import it.pagopa.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.errors.GenericComponentErrors.OperationForbidden
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.interop.purposemanagement.client.model.{PurposeVersionState => DepPurposeVersionState}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.catalogmanagement.client.{model => CatalogManagementDependency}
import it.pagopa.interop.purposeprocess.api.PurposeApiService
import it.pagopa.interop.purposeprocess.api.converters._
import it.pagopa.interop.purposeprocess.api.converters.agreementmanagement.{AgreementConverter, ProblemConverter}
import it.pagopa.interop.purposeprocess.api.converters.authorizationmanagement.ClientConverter
import it.pagopa.interop.purposeprocess.api.converters.catalogmanagement.EServiceConverter
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement._
import it.pagopa.interop.purposeprocess.api.converters.tenantmanagement.OrganizationConverter
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.interop.purposeprocess.error.InternalErrors.{
  OrganizationIsNotTheConsumer,
  OrganizationIsNotTheProducer,
  OrganizationNotAllowed,
  RiskAnalysisValidationFailed
}
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposeprocess.service._

import java.io.File
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

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
)(implicit ec: ExecutionContext)
    extends PurposeApiService {

  private val logger = Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  private[this] def authorize(roles: String*)(
    route: => Route
  )(implicit contexts: Seq[(String, String)], toEntityMarshallerProblem: ToEntityMarshaller[Problem]): Route =
    authorizeInterop(hasPermissions(roles: _*), problemOf(StatusCodes.Forbidden, OperationForbidden)) {
      route
    }

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
  ): Route = {
    val result: Future[HttpEntity.Strict] = for {
      purposeUUID  <- purposeId.toFutureUUID
      versionUUID  <- versionId.toFutureUUID
      documentUUID <- documentId.toFutureUUID
      purpose      <- purposeManagementService.getPurpose(purposeUUID)
      version      <- purpose.versions.find(_.id == versionUUID).toFuture(PurposeVersionNotFound(purposeId, versionId))
      document     <- version.riskAnalysis
        .find(_.id == documentUUID)
        .toFuture(PurposeVersionDocumentNotFound(purposeId, versionId, documentId))
      byteStream   <- fileManager.get(ApplicationConfiguration.storageContainer)(document.path)
    } yield HttpEntity(ContentType(MediaTypes.`application/pdf`), byteStream.toByteArray)

    val defaultProblem: Problem =
      problemOf(StatusCodes.BadRequest, GetPurposeVersionDocumentBadRequest(purposeId, versionId, documentId))

    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(response)                          => complete(response)
        case Failure(e: PurposeVersionNotFound)         =>
          logger.error("Error while downloading Risk Analysis Document", e)
          getRiskAnalysisDocument404(problemOf(StatusCodes.NotFound, e))
        case Failure(e: PurposeVersionDocumentNotFound) =>
          logger.error("Error while downloading Risk Analysis Document", e)
          getRiskAnalysisDocument404(problemOf(StatusCodes.NotFound, e))
        case Failure(e)                                 =>
          logger.error("Error while downloading Risk Analysis Document", e)
          getRiskAnalysisDocument400(defaultProblem)
      }
    }
  }

  override def createPurpose(seed: PurposeSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    logger.info("Creating Purpose {}", seed)
    val result: Future[Purpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      ownership      <- assertOrganizationIsAConsumer(organizationId, seed.consumerId)
      clientSeed     <- PurposeSeedConverter.apiToDependency(seed).toFuture
      agreements     <- agreementManagementService.getAgreements(seed.eserviceId, seed.consumerId)
      _        <- agreements.headOption.toFuture(AgreementNotFound(seed.eserviceId.toString, seed.consumerId.toString))
      purpose  <- purposeManagementService.createPurpose(clientSeed)
      eService <- catalogManagementService.getEServiceById(purpose.eserviceId)
      result   <- enhancePurpose(purpose, eService, ownership)
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(purpose)                          =>
          createPurpose201(purpose)
        case Failure(ex: RiskAnalysisValidationFailed) =>
          logger.error(s"Error creating purpose - Risk Analysis Validation failed $seed", ex)
          val problem = problemOf(StatusCodes.BadRequest, RiskAnalysisFormError(ex.getMessage))
          createPurpose400(problem)
        case Failure(ex)                               =>
          logger.error(s"Error creating purpose $seed ", ex)
          createPurpose400(defaultProblem)
      }
    }
  }

  override def createPurposeVersion(purposeId: String, seed: PurposeVersionSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    logger.info("Creating Purpose Version {}", seed)
    val result: Future[PurposeVersion] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      depSeed = PurposeVersionSeedConverter.apiToDependency(seed)
      version <- purposeManagementService.createPurposeVersion(purposeUUID, depSeed)
    } yield PurposeVersionConverter.dependencyToApi(version)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeVersionBadRequest(purposeId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(purpose) =>
          createPurposeVersion201(purpose)
        case Failure(ex)      =>
          logger.error(s"Error creating purpose version $seed ", ex)
          createPurposeVersion400(defaultProblem)
      }
    }
  }

  override def updatePurpose(purposeId: String, purposeUpdateContent: PurposeUpdateContent)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    logger.info("Updating Purpose {}", purposeId)
    val result: Future[Purpose] = for {
      organizationId  <- getOrganizationIdFutureUUID(contexts)
      purposeUUID     <- purposeId.toFutureUUID
      purpose         <- purposeManagementService.getPurpose(purposeUUID)
      ownership       <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      depPayload      <- PurposeUpdateContentConverter.apiToDependency(purposeUpdateContent).toFuture
      updatedPurpose  <- purposeManagementService.updatePurpose(purposeUUID, depPayload)
      eService        <- catalogManagementService.getEServiceById(updatedPurpose.eserviceId)
      enhancedPurpose <- enhancePurpose(updatedPurpose, eService, ownership)
    } yield enhancedPurpose

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, UpdatePurposeBadRequest(purposeId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(purpose)                          =>
          updatePurpose200(purpose)
        case Failure(ex: RiskAnalysisValidationFailed) =>
          logger.error(s"Error Updating Purpose $purposeId - Risk Analysis Validation failed", ex)
          val problem = problemOf(StatusCodes.BadRequest, RiskAnalysisFormError(ex.getMessage))
          createPurpose400(problem)
        case Failure(ex)                               =>
          logger.error(s"Error updating Purpose $purposeId", ex)
          updatePurpose400(defaultProblem)
      }
    }
  }

  override def getPurpose(id: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, M2M_ROLE) {
    logger.info("Retrieving Purpose {}", id)
    val result: Future[Purpose] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      uuid           <- id.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(uuid)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership      <- Ownership
        .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
        .toFuture
      result         <- enhancePurpose(purpose, eService, ownership)
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, GetPurposeBadRequest(id))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(purpose) =>
          getPurpose200(purpose)
        case Failure(ex)      =>
          logger.error(s"Error while retrieving purpose $id", ex)
          getPurpose400(defaultProblem)
      }
    }
  }

  override def getPurposes(eServiceId: Option[String], consumerId: Option[String], states: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposes: ToEntityMarshaller[Purposes],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE, M2M_ROLE) {
    logger.info("Retrieving Purposes for EService {}, Consumer {} and States {}", eServiceId, consumerId, states)

    def filterPurposeByOrganizationRole(
      purposes: PurposeManagementDependency.Purposes,
      organizationId: UUID
    ): Future[Seq[Purpose]] = {
      // TODO Bad. This would lead to inconsistent page size during pagination.
      //      Can we use filter parameters on management request?
      for {
        ps <- Future
          .traverse(purposes.purposes)(purpose =>
            for {
              eService        <- catalogManagementService.getEServiceById(purpose.eserviceId)
              ownership       <- Ownership
                .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
                .toFuture
                .map(Some(_))
                .recover(_ => None)
              enhancedPurpose <- ownership.traverse(enhancePurpose(purpose, eService, _))
            } yield enhancedPurpose
          )
      } yield ps.flatten
    }

    val result: Future[Purposes] = for {
      eServiceUUID      <- eServiceId.traverse(_.toFutureUUID)
      consumerUUID      <- consumerId.traverse(_.toFutureUUID)
      organizationId    <- getOrganizationIdFutureUUID(contexts)
      states            <- parseArrayParameters(states).traverse(DepPurposeVersionState.fromValue).toFuture
      purposes          <- purposeManagementService.getPurposes(eServiceUUID, consumerUUID, states)
      convertedPurposes <- filterPurposeByOrganizationRole(purposes, organizationId)
    } yield Purposes(purposes = convertedPurposes)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, GetPurposesBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
        case Success(purpose) =>
          getPurposes200(purpose)
        case Failure(ex)      =>
          logger.error(
            s"Error while retrieving purposes for EService $eServiceId, Consumer $consumerId and States $states",
            ex
          )
          getPurposes400(defaultProblem)
      }
    }
  }

  override def deletePurpose(
    id: String
  )(implicit contexts: Seq[(String, String)], toEntityMarshallerProblem: ToEntityMarshaller[Problem]): Route =
    authorize(ADMIN_ROLE) {
      logger.info("Attempting to delete purpose {}", id)

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
        _              <- Future.successful(purpose).ensure(UndeletableVersionError(id))(isDeletable)
        clients        <- authorizationManagementService.getClients(Some(purposeUUID))
        _              <- Future.traverse(clients)(client =>
          authorizationManagementService.removePurposeFromClient(purposeUUID, client.id)
        )
        _              <- Future.traverse(purpose.versions)(version =>
          purposeManagementService.deletePurposeVersion(purposeUUID, version.id)
        )
        _              <- purposeManagementService.deletePurpose(purposeUUID)
      } yield ()

      val defaultProblem: Problem = problemOf(StatusCodes.InternalServerError, DeletePurposeBadRequest)

      onComplete(result) {
        handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
          case Success(_)                           => deletePurpose204
          case Failure(ex: UndeletableVersionError) =>
            logger.error(s"Error while deleting purpose $id", ex)
            deletePurpose403(problemOf(StatusCodes.Forbidden, ex))
          case Failure(ex)                          =>
            logger.error(s"Error while deleting purpose $id", ex)
            complete(StatusCodes.InternalServerError, defaultProblem)
        }
      }
    }

  override def deletePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    logger.info(s"Attempting to delete version $versionId of purpose $purposeId")

    val result: Future[Unit] = for {
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      _              <- purposeManagementService.deletePurposeVersion(purposeUUID, versionUUID)
    } yield ()

    val defaultProblem: Problem =
      problemOf(StatusCodes.InternalServerError, DeletePurposeVersionError(purposeId, versionId))

    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(_)  => deletePurposeVersion204
        case Failure(ex) =>
          logger.error(s"Error while deleting version $versionId of purpose $purposeId", ex)
          complete(StatusCodes.InternalServerError, defaultProblem)
      }
    }
  }

  override def activatePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    logger.info("Activating Version {} of Purpose {}", versionId, purposeId)
    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      version        <- purpose.versions
        .find(_.id == versionUUID)
        .toFuture(ActivatePurposeVersionNotFound(purposeId, versionId))
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

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, ActivatePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(result)                             =>
          activatePurposeVersion200(result)
        case Failure(ex: ActivatePurposeVersionNotFound) =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId", ex)
          val problem = problemOf(StatusCodes.NotFound, ex)
          activatePurposeVersion404(problem)
        case Failure(ex: AgreementNotFound)              =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId", ex)
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex: DescriptorNotFound)             =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId", ex)
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex: MissingRiskAnalysis)            =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId", ex)
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex)                                 =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId", ex)
          activatePurposeVersion400(defaultProblem)
      }
    }
  }

  override def suspendPurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    logger.info("Suspending Version {} of Purpose {}", versionId, purposeId)
    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      eService       <- catalogManagementService.getEServiceById(purpose.eserviceId)
      ownership      <- Ownership
        .getOrganizationRole(organizationId, eService.producerId, purpose.consumerId)
        .toFuture
      stateDetails = PurposeManagementDependency.StateChangeDetails(ownership.toChangedBy)
      response <- purposeManagementService.suspendPurposeVersion(purposeUUID, versionUUID, stateDetails)
      _        <- authorizationManagementService.updateStateOnClients(
        purposeId = purposeUUID,
        versionId = versionUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, SuspendPurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(r)  =>
          suspendPurposeVersion200(r)
        case Failure(ex) =>
          logger.error(s"Error while suspending Version $versionId of Purpose $purposeId", ex)
          suspendPurposeVersion400(defaultProblem)
      }
    }
  }

  override def archivePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = authorize(ADMIN_ROLE) {
    logger.info("Archiving for Version {} of Purpose {}", versionId, purposeId)
    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      stateDetails = PurposeManagementDependency.StateChangeDetails(PurposeManagementDependency.ChangedBy.CONSUMER)
      response <- purposeManagementService.archivePurposeVersion(purposeUUID, versionUUID, stateDetails)
      _        <- authorizationManagementService.updateStateOnClients(
        purposeId = purposeUUID,
        versionId = versionUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(r)  =>
          archivePurposeVersion200(r)
        case Failure(ex) =>
          logger.error(s"Error while archiving Version $versionId of Purpose $purposeId", ex)
          archivePurposeVersion400(defaultProblem)
      }
    }
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
    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAConsumer(organizationId, purpose.consumerId)
      update = DraftPurposeVersionUpdateContentConverter.apiToDependency(updateContent)
      response <- purposeManagementService.updateDraftPurposeVersion(purposeUUID, versionUUID, update)
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem =
      problemOf(StatusCodes.InternalServerError, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(r)  =>
          updateDraftPurposeVersion200(r)
        case Failure(ex) =>
          logger.error(s"Error while updating draft Version $versionId of Purpose $purposeId", ex)
          complete(StatusCodes.InternalServerError, defaultProblem)
      }
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
    val result: Future[PurposeVersion] = for {
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      organizationId <- getOrganizationIdFutureUUID(contexts)
      purpose        <- purposeManagementService.getPurpose(purposeUUID)
      _              <- assertOrganizationIsAProducer(organizationId, purpose.eserviceId)
      update = WaitingForApprovalPurposeVersionUpdateContentConverter.apiToDependency(updateContent)
      response <- purposeManagementService.updateWaitingForApprovalPurposeVersion(purposeUUID, versionUUID, update)
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem =
      problemOf(StatusCodes.InternalServerError, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleOrganizationRoleError orElse {
        case Success(r)  =>
          updateWaitingForApprovalPurposeVersion200(r)
        case Failure(ex) =>
          logger.error(s"Error while updating waiting for approval Version $versionId of Purpose $purposeId", ex)
          complete(StatusCodes.InternalServerError, defaultProblem)
      }
    }
  }

  def assertOrganizationIsAConsumer(organizationId: UUID, consumerId: UUID): Future[Ownership] =
    if (organizationId == consumerId) Future.successful(Ownership.CONSUMER)
    else Future.failed(OrganizationIsNotTheConsumer(organizationId))

  def assertOrganizationIsAProducer(organizationId: UUID, eServiceId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Ownership] =
    for {
      eService <- catalogManagementService.getEServiceById(eServiceId)
      _ <- Future.failed(OrganizationIsNotTheProducer(organizationId)).unlessA(organizationId == eService.producerId)
    } yield Ownership.PRODUCER

  def enhancePurpose(
    depPurpose: PurposeManagementDependency.Purpose,
    eService: CatalogManagementDependency.EService,
    ownership: Ownership
  )(implicit contexts: Seq[(String, String)]): Future[Purpose] = {
    def clientsByUserType(): Future[List[Client]] =
      ownership match {
        case Ownership.PRODUCER                           => List.empty[Client].pure[Future]
        case Ownership.CONSUMER | Ownership.SELF_CONSUMER =>
          authorizationManagementService
            .getClients(purposeId = depPurpose.id.some)
            .map(_.toList.map(ClientConverter.dependencyToApi))
      }

    for {
      depAgreements <- agreementManagementService.getAgreements(depPurpose.eserviceId, depPurpose.consumerId)
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
      .dependencyToApi(
        purpose = depPurpose,
        eService = eService,
        agreement = agreement,
        consumer = consumer,
        clients = clients
      )
  }

  def handleApiError(
    defaultProblem: Problem
  )(implicit contexts: Seq[(String, String)]): PartialFunction[Try[_], StandardRoute] = {
    case Failure(err: AgreementApiError[_])     =>
      logger.error("Error received from Agreement Management - {}", err.responseContent)
      val problem = err.responseContent match {
        case Some(body: String) => ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: AuthorizationApiError[_]) =>
      logger.error("Error received from Authorization Management - {}", err.responseContent)
      val problem = err.responseContent match {
        case Some(body: String) => authorizationmanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: CatalogApiError[_])       =>
      logger.error("Error received from Catalog Management - {}", err.responseContent)
      val problem = err.responseContent match {
        case Some(body: String) => catalogmanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: PurposeApiError[_])       =>
      logger.error("Error received from Purpose Management - {}", err.responseContent)
      val problem = err.responseContent match {
        case Some(body: String) => purposemanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
  }

  def handleOrganizationRoleError(implicit contexts: Seq[(String, String)]): PartialFunction[Try[_], StandardRoute] = {
    case Failure(err: OrganizationIsNotTheConsumer) =>
      logger.error("The action can be performed only by a Consumer. User {}", err.organizationId)
      val problem = problemOf(StatusCodes.Forbidden, OnlyConsumerAllowedError)
      complete(problem.status, problem)
    case Failure(MissingSelfcareId)                 =>
      logger.error(s"Error while retrieving selfcareId from tenant", MissingSelfcareId)
      complete(StatusCodes.InternalServerError, problemOf(StatusCodes.InternalServerError, MissingSelfcareId))
    case Failure(err: OrganizationIsNotTheProducer) =>
      logger.error("The action can be performed only by a Producer. User {}", err.organizationId)
      val problem = problemOf(StatusCodes.Forbidden, OnlyProducerAllowedError)
      complete(problem.status, problem)
    case Failure(err: OrganizationNotAllowed)       =>
      logger.error("Organization is neither a Consumer or a Producer. User {}", err.organizationId)
      val problem = problemOf(StatusCodes.Forbidden, OrganizationNotAllowedError)
      complete(problem.status, problem)
  }
}
