package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.{Route, StandardRoute}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import it.pagopa.interop.agreementmanagement.client.invoker.{ApiError => AgreementApiError}
import it.pagopa.interop.authorizationmanagement.client.invoker.{ApiError => AuthorizationApiError}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagementDependency}
import it.pagopa.interop.catalogmanagement.client.invoker.{ApiError => CatalogApiError}
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.commons.utils.AkkaUtils.{getBearer, getUidFuture}
import it.pagopa.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.partymanagement.client.invoker.{ApiError => PartyApiError}
import it.pagopa.interop.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.interop.purposemanagement.client.model.{PurposeVersionState => DepPurposeVersionState}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.purposeprocess.api.PurposeApiService
import it.pagopa.interop.purposeprocess.api.converters.agreementmanagement.{AgreementConverter, ProblemConverter}
import it.pagopa.interop.purposeprocess.api.converters._
import it.pagopa.interop.purposeprocess.api.converters.authorizationmanagement.ClientConverter
import it.pagopa.interop.purposeprocess.api.converters.catalogmanagement.EServiceConverter
import it.pagopa.interop.purposeprocess.api.converters.partymanagement.OrganizationConverter
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement._
import it.pagopa.interop.purposeprocess.error.InternalErrors.{
  RiskAnalysisValidationFailed,
  UserIsNotTheConsumer,
  UserIsNotTheProducer,
  UserNotAllowed
}
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposeprocess.service._
import org.slf4j.LoggerFactory

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

final case class PurposeApiServiceImpl(
  agreementManagementService: AgreementManagementService,
  authorizationManagementService: AuthorizationManagementService,
  catalogManagementService: CatalogManagementService,
  partyManagementService: PartyManagementService,
  purposeManagementService: PurposeManagementService,
  fileManager: FileManager,
  pdfCreator: PDFCreator,
  uuidSupplier: UUIDSupplier,
  dateTimeSupplier: OffsetDateTimeSupplier
)(implicit ec: ExecutionContext)
    extends PurposeApiService {

  private val logger = Logger.takingImplicit[ContextFieldsToLog](LoggerFactory.getLogger(this.getClass))

  private[this] val purposeVersionActivation = PurposeVersionActivation(
    agreementManagementService,
    authorizationManagementService,
    purposeManagementService,
    fileManager,
    pdfCreator,
    uuidSupplier,
    dateTimeSupplier
  )

  override def createPurpose(seed: PurposeSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Creating Purpose {}", seed)
    val result: Future[Purpose] = for {
      bearerToken <- getBearer(contexts).toFuture
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      clientSeed  <- PurposeSeedConverter.apiToDependency(seed).toFuture
      userType    <- assertUserIsAConsumer(bearerToken)(userUUID, seed.consumerId)
      agreements  <- agreementManagementService.getAgreements(contexts)(seed.eserviceId, seed.consumerId)
      _       <- agreements.headOption.toFuture(AgreementNotFound(seed.eserviceId.toString, seed.consumerId.toString))
      purpose <- purposeManagementService.createPurpose(contexts)(clientSeed)
      result  <- enhancePurpose(bearerToken, contexts)(purpose, userType)
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(purpose)                          =>
          createPurpose201(purpose)
        case Failure(ex: RiskAnalysisValidationFailed) =>
          logger.error(s"Error creating purpose - Risk Analysis Validation failed $seed - ${ex.getMessage}")
          val problem = problemOf(StatusCodes.BadRequest, RiskAnalysisFormError(ex.getMessage))
          createPurpose400(problem)
        case Failure(ex)                               =>
          logger.error(s"Error creating purpose $seed  - ${ex.getMessage}")
          createPurpose400(defaultProblem)
      }
    }
  }

  override def createPurposeVersion(purposeId: String, seed: PurposeVersionSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Creating Purpose Version {}", seed)
    val result: Future[PurposeVersion] = for {
      bearerToken <- getBearer(contexts).toFuture
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purposeUUID <- purposeId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      depSeed = PurposeVersionSeedConverter.apiToDependency(seed)
      version <- purposeManagementService.createPurposeVersion(contexts)(purposeUUID, depSeed)
    } yield PurposeVersionConverter.dependencyToApi(version)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeVersionBadRequest(purposeId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(purpose) =>
          createPurposeVersion201(purpose)
        case Failure(ex)      =>
          logger.error(s"Error creating purpose version $seed  - ${ex.getMessage}")
          createPurposeVersion400(defaultProblem)
      }
    }
  }

  override def updatePurpose(purposeId: String, purposeUpdateContent: PurposeUpdateContent)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Updating Purpose {}", purposeId)
    val result: Future[Purpose] = for {
      bearerToken     <- getBearer(contexts).toFuture
      userId          <- getUidFuture(contexts)
      userUUID        <- userId.toFutureUUID
      purposeUUID     <- purposeId.toFutureUUID
      purpose         <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      userType        <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      depPayload      <- PurposeUpdateContentConverter.apiToDependency(purposeUpdateContent).toFuture
      updatedPurpose  <- purposeManagementService.updatePurpose(contexts)(purposeUUID, depPayload)
      enhancedPurpose <- enhancePurpose(bearerToken, contexts)(updatedPurpose, userType)
    } yield enhancedPurpose

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, UpdatePurposeBadRequest(purposeId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(purpose)                          =>
          updatePurpose200(purpose)
        case Failure(ex: RiskAnalysisValidationFailed) =>
          logger.error(s"Error Updating Purpose $purposeId - Risk Analysis Validation failed - ${ex.getMessage}")
          val problem = problemOf(StatusCodes.BadRequest, RiskAnalysisFormError(ex.getMessage))
          createPurpose400(problem)
        case Failure(ex)                               =>
          logger.error(s"Error updating Purpose $purposeId - ${ex.getMessage}")
          updatePurpose400(defaultProblem)
      }
    }
  }

  override def getPurpose(id: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Retrieving Purpose {}", id)
    val result: Future[Purpose] = for {
      bearerToken <- getBearer(contexts).toFuture
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      uuid        <- id.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(uuid)
      userType    <- userType(bearerToken, contexts)(userUUID, purpose.eserviceId, purpose.consumerId)
      result      <- enhancePurpose(bearerToken, contexts)(purpose, userType)
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, GetPurposeBadRequest(id))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(purpose) =>
          getPurpose200(purpose)
        case Failure(ex)      =>
          logger.error(s"Error while retrieving purpose $id - ${ex.getMessage}")
          getPurpose400(defaultProblem)
      }
    }
  }

  override def getPurposes(eServiceId: Option[String], consumerId: Option[String], states: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposes: ToEntityMarshaller[Purposes],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Retrieving Purposes for EService {}, Consumer {} and States {}", eServiceId, consumerId, states)

    def filterPurposeByUserType(
      bearerToken: String
    )(purposes: PurposeManagementDependency.Purposes, userId: UUID): Future[Seq[Purpose]] = {
      // TODO Bad. This would lead to inconsistent page size during pagination.
      //      Can we use filter parameters on management request?
      purposes.purposes
        .traverse(purpose =>
          for {
            userType        <- userType(bearerToken, contexts)(userId, purpose.eserviceId, purpose.consumerId)
              .map(Some(_))
              .recover(_ => None)
            enhancedPurpose <- userType.traverse(enhancePurpose(bearerToken, contexts)(purpose, _))
          } yield enhancedPurpose
        )
        .map(_.flatten)
    }

    val result: Future[Purposes] = for {
      bearerToken       <- getBearer(contexts).toFuture
      eServiceUUID      <- eServiceId.traverse(_.toFutureUUID)
      consumerUUID      <- consumerId.traverse(_.toFutureUUID)
      userId            <- getUidFuture(contexts)
      userUUID          <- userId.toFutureUUID
      states            <- parseArrayParameters(states).traverse(DepPurposeVersionState.fromValue).toFuture
      purposes          <- purposeManagementService.getPurposes(contexts)(eServiceUUID, consumerUUID, states)
      convertedPurposes <- filterPurposeByUserType(bearerToken)(purposes, userUUID)
    } yield Purposes(purposes = convertedPurposes)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, GetPurposesBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
        case Success(purpose) =>
          getPurposes200(purpose)
        case Failure(ex)      =>
          logger.error(
            s"Error while retrieving purposes for EService $eServiceId, Consumer $consumerId and States $states - ${ex.getMessage}"
          )
          getPurposes400(defaultProblem)
      }
    }
  }

  override def deletePurpose(
    id: String
  )(implicit contexts: Seq[(String, String)], toEntityMarshallerProblem: ToEntityMarshaller[Problem]): Route = {
    logger.info("Attempting to delete purpose {}", id)

    def isDeletable(purpose: PurposeManagementDependency.Purpose): Boolean = {
      val states = purpose.versions.map(_.state)
      states.isEmpty ||
      states == Seq(PurposeManagementDependency.PurposeVersionState.DRAFT) ||
      states == Seq(PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL)
    }

    val result: Future[Unit] = for {
      bearerToken <- getBearer(contexts).toFuture
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purposeUUID <- id.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      _           <- Future.successful(purpose).ensure(UndeletableVersionError(id))(isDeletable)
      clients     <- authorizationManagementService.getClients(contexts)(Some(purposeUUID))
      _           <- clients.traverse(client =>
        authorizationManagementService.removePurposeFromClient(contexts)(purposeUUID, client.id)
      )
      _           <- purpose.versions.traverse(version =>
        purposeManagementService.deletePurposeVersion(contexts)(purposeUUID, version.id)
      )
      _           <- purposeManagementService.deletePurpose(contexts)(purposeUUID)
    } yield ()

    val defaultProblem: Problem = problemOf(StatusCodes.InternalServerError, DeletePurposeBadRequest)

    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(_)                           => deletePurpose204
        case Failure(ex: UndeletableVersionError) =>
          logger.error(s"Error while deleting purpose $id - ${ex.getMessage}")
          deletePurpose403(problemOf(StatusCodes.Forbidden, ex))
        case Failure(ex)                          =>
          logger.error(s"Error while deleting purpose $id - ${ex.getMessage}")
          complete(StatusCodes.InternalServerError, defaultProblem)
      }
    }
  }

  override def deletePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info(s"Attempting to delete version $versionId of purpose $purposeId")

    val result: Future[Unit] = for {
      bearerToken <- getBearer(contexts).toFuture
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      _           <- purposeManagementService.deletePurposeVersion(contexts)(purposeUUID, versionUUID)
    } yield ()

    val defaultProblem: Problem =
      problemOf(StatusCodes.InternalServerError, DeletePurposeVersionError(purposeId, versionId))

    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(_)  => deletePurposeVersion204
        case Failure(ex) =>
          logger.error(s"Error while deleting version $versionId of purpose $purposeId - ${ex.getMessage}")
          complete(StatusCodes.InternalServerError, defaultProblem)
      }
    }
  }

  override def activatePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Activating Version {} of Purpose {}", versionId, purposeId)
    val result: Future[PurposeVersion] = for {
      bearerToken    <- getBearer(contexts).toFuture
      purposeUUID    <- purposeId.toFutureUUID
      versionUUID    <- versionId.toFutureUUID
      userId         <- getUidFuture(contexts)
      userUUID       <- userId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      version        <- purpose.versions
        .find(_.id == versionUUID)
        .toFuture(ActivatePurposeVersionNotFound(purposeId, versionId))
      userType       <- userType(bearerToken, contexts)(userUUID, purpose.eserviceId, purpose.consumerId)
      eService       <- catalogManagementService.getEServiceById(contexts)(purpose.eserviceId)
      updatedVersion <- purposeVersionActivation.activateOrWaitForApproval(contexts)(
        eService,
        purpose,
        version,
        userType,
        userUUID
      )
    } yield PurposeVersionConverter.dependencyToApi(updatedVersion)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, ActivatePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(result)                             =>
          activatePurposeVersion200(result)
        case Failure(ex: ActivatePurposeVersionNotFound) =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId - ${ex.getMessage}")
          val problem = problemOf(StatusCodes.NotFound, ex)
          activatePurposeVersion404(problem)
        case Failure(ex: AgreementNotFound)              =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId - ${ex.getMessage}")
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex: DescriptorNotFound)             =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId - ${ex.getMessage}")
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex: MissingRiskAnalysis)            =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId - ${ex.getMessage}")
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex)                                 =>
          logger.error(s"Error while activating Version $versionId of Purpose $purposeId - ${ex.getMessage}")
          activatePurposeVersion400(defaultProblem)
      }
    }
  }

  override def suspendPurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Suspending Version {} of Purpose {}", versionId, purposeId)
    val result: Future[PurposeVersion] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      userType    <- userType(bearerToken, contexts)(userUUID, purpose.eserviceId, purpose.consumerId)
      stateDetails = PurposeManagementDependency.StateChangeDetails(userType)
      response <- purposeManagementService.suspendPurposeVersion(contexts)(purposeUUID, versionUUID, stateDetails)
      _        <- authorizationManagementService.updateStateOnClients(contexts)(
        purposeId = purposeUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, SuspendPurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(r)  =>
          suspendPurposeVersion200(r)
        case Failure(ex) =>
          logger.error(s"Error while suspending Version $versionId of Purpose $purposeId - ${ex.getMessage}")
          suspendPurposeVersion400(defaultProblem)
      }
    }
  }

  override def archivePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Archiving for Version {} of Purpose {}", versionId, purposeId)
    val result: Future[PurposeVersion] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      stateDetails = PurposeManagementDependency.StateChangeDetails(PurposeManagementDependency.ChangedBy.CONSUMER)
      response <- purposeManagementService.archivePurposeVersion(contexts)(purposeUUID, versionUUID, stateDetails)
      _        <- authorizationManagementService.updateStateOnClients(contexts)(
        purposeId = purposeUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(r)  =>
          archivePurposeVersion200(r)
        case Failure(ex) =>
          logger.error(s"Error while archiving Version $versionId of Purpose $purposeId - ${ex.getMessage}")
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
  ): Route = {
    val result: Future[PurposeVersion] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      update = DraftPurposeVersionUpdateContentConverter.apiToDependency(updateContent)
      response <- purposeManagementService.updateDraftPurposeVersion(contexts)(purposeUUID, versionUUID, update)
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem =
      problemOf(StatusCodes.InternalServerError, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(r)  =>
          updateDraftPurposeVersion200(r)
        case Failure(ex) =>
          logger.error(s"Error while updating draft Version $versionId of Purpose $purposeId - ${ex.getMessage}")
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
  ): Route = {
    val result: Future[PurposeVersion] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(contexts)(purposeUUID)
      _           <- assertUserIsAProducer(bearerToken, contexts)(userUUID, purpose.eserviceId)
      update = WaitingForApprovalPurposeVersionUpdateContentConverter.apiToDependency(updateContent)
      response <- purposeManagementService.updateWaitingForApprovalPurposeVersion(contexts)(
        purposeUUID,
        versionUUID,
        update
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem =
      problemOf(StatusCodes.InternalServerError, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(r)  =>
          updateWaitingForApprovalPurposeVersion200(r)
        case Failure(ex) =>
          logger.error(
            s"Error while updating waiting for approval Version $versionId of Purpose $purposeId - ${ex.getMessage}"
          )
          complete(StatusCodes.InternalServerError, defaultProblem)
      }
    }
  }

  // TODO This may not work as expected if the user has an active relationship with
  //      both producer and consumer
  def userType(
    bearerToken: String,
    contexts: Seq[(String, String)]
  )(userId: UUID, eServiceId: UUID, consumerId: UUID): Future[PurposeManagementDependency.ChangedBy] =
    assertUserIsAConsumer(bearerToken)(userId, consumerId)
      .recoverWith(_ => assertUserIsAProducer(bearerToken, contexts)(userId, eServiceId))
      .recoverWith(_ => Future.failed(UserNotAllowed(userId)))

  // TODO This may not work as expected if the user has an active relationship with
  //      both producer and consumer
  def assertUserIsAConsumer(
    bearerToken: String
  )(userId: UUID, consumerId: UUID): Future[PurposeManagementDependency.ChangedBy] =
    for {
      relationships <- partyManagementService.getActiveRelationships(bearerToken)(userId, consumerId)
      _             <- Either.cond(relationships.items.nonEmpty, (), UserIsNotTheConsumer(userId)).toFuture
    } yield PurposeManagementDependency.ChangedBy.CONSUMER

  // TODO This may not work as expected if the user has an active relationship with
  //      both producer and consumer
  def assertUserIsAProducer(
    bearerToken: String,
    contexts: Seq[(String, String)]
  )(userId: UUID, eServiceId: UUID): Future[PurposeManagementDependency.ChangedBy] =
    for {
      eService      <- catalogManagementService.getEServiceById(contexts)(eServiceId)
      relationships <- partyManagementService.getActiveRelationships(bearerToken)(userId, eService.producerId)
      _             <- Either.cond(relationships.items.nonEmpty, (), UserIsNotTheProducer(userId)).toFuture
    } yield PurposeManagementDependency.ChangedBy.PRODUCER

  def enhancePurpose(bearerToken: String, contexts: Seq[(String, String)])(
    depPurpose: PurposeManagementDependency.Purpose,
    userType: PurposeManagementDependency.ChangedBy
  ): Future[Purpose] = {
    def clientsByUserType(): Future[Seq[Client]] =
      userType match {
        case PurposeManagementDependency.ChangedBy.PRODUCER => Future.successful(Seq.empty[Client])
        case PurposeManagementDependency.ChangedBy.CONSUMER =>
          for {
            depClients <- authorizationManagementService.getClients(contexts)(purposeId = Some(depPurpose.id))
            clients = depClients.map(ClientConverter.dependencyToApi)
          } yield clients
      }

    for {
      depAgreements <- agreementManagementService.getAgreements(contexts)(depPurpose.eserviceId, depPurpose.consumerId)
      depAgreement  <- depAgreements
        .sortBy(_.createdAt)
        .lastOption
        .toFuture(AgreementNotFound(depPurpose.eserviceId.toString, depPurpose.consumerId.toString))
      depEService   <- catalogManagementService.getEServiceById(contexts)(depPurpose.eserviceId)
      depProducer   <- partyManagementService.getInstitutionById(bearerToken)(depEService.producerId)
      agreement = AgreementConverter.dependencyToApi(depAgreement)
      producer  = OrganizationConverter.dependencyToApi(depProducer)
      eService <- EServiceConverter.dependencyToApi(depEService, depAgreement.descriptorId, producer).toFuture
      clients  <- clientsByUserType()
      purpose  <- PurposeConverter
        .dependencyToApi(purpose = depPurpose, eService = eService, agreement = agreement, clients = clients)
        .toFuture
    } yield purpose
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
    case Failure(err: PartyApiError[_])         =>
      logger.error("Error received from Party Management - {}", err.responseContent)
      val problem = err.responseContent match {
        case Some(body: String) => partymanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
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

  def handleUserTypeError(implicit contexts: Seq[(String, String)]): PartialFunction[Try[_], StandardRoute] = {
    case Failure(err: UserIsNotTheConsumer) =>
      logger.error("The action can be performed only by a Consumer. User {}", err.userId)
      val problem = problemOf(StatusCodes.Forbidden, OnlyConsumerAllowedError)
      complete(problem.status, problem)
    case Failure(err: UserIsNotTheProducer) =>
      logger.error("The action can be performed only by a Producer. User {}", err.userId)
      val problem = problemOf(StatusCodes.Forbidden, OnlyProducerAllowedError)
      complete(problem.status, problem)
    case Failure(err: UserNotAllowed)       =>
      logger.error("User is neither a Consumer or a Producer. User {}", err.userId)
      val problem = problemOf(StatusCodes.Forbidden, UserNotAllowedError)
      complete(problem.status, problem)
  }
}
