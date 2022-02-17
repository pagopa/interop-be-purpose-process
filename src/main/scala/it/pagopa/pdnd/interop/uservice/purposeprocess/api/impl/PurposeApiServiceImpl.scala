package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.{Route, StandardRoute}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import it.pagopa.pdnd.interop.commons.files.service.FileManager
import it.pagopa.pdnd.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.pdnd.interop.commons.utils.AkkaUtils.{getBearer, getUidFuture}
import it.pagopa.pdnd.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.pdnd.interop.commons.utils.TypeConversions._
import it.pagopa.pdnd.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.invoker.{ApiError => CatalogApiError}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.invoker.{ApiError => PartyApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  ChangedBy,
  PurposeVersionState => DepPurposeVersionState
}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.uservice.keymanagement.client.{model => AuthorizationManagementDependency}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement._
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.InternalErrors.{
  RiskAnalysisValidationFailed,
  UserIsNotTheConsumer,
  UserIsNotTheProducer,
  UserNotAllowed
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import it.pagopa.pdnd.interop.uservice.purposeprocess.service._
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
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, seed.consumerId)
      agreements  <- agreementManagementService.getAgreements(bearerToken)(seed.eserviceId, seed.consumerId)
      _           <- agreements.headOption.toFuture(AgreementNotFound(seed.eserviceId.toString, seed.consumerId.toString))
      purpose     <- purposeManagementService.createPurpose(bearerToken)(clientSeed)
      result      <- PurposeConverter.dependencyToApi(purpose).toFuture
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(purpose) =>
          createPurpose201(purpose)
        case Failure(ex: RiskAnalysisValidationFailed) =>
          logger.error("Error creating purpose - Risk Analysis Validation failed {} - {}", seed, ex.getMessage)
          val problem = problemOf(StatusCodes.BadRequest, RiskAnalysisFormError(ex.getMessage))
          createPurpose400(problem)
        case Failure(ex) =>
          logger.error("Error creating purpose {}", seed, ex)
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
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      depSeed = PurposeVersionSeedConverter.apiToDependency(seed)
      version <- purposeManagementService.createPurposeVersion(bearerToken)(purposeUUID, depSeed)
    } yield PurposeVersionConverter.dependencyToApi(version)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeVersionBadRequest(purposeId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(purpose) =>
          createPurposeVersion201(purpose)
        case Failure(ex) =>
          logger.error("Error creating purpose version {}", seed, ex)
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
      bearerToken    <- getBearer(contexts).toFuture
      userId         <- getUidFuture(contexts)
      userUUID       <- userId.toFutureUUID
      purposeUUID    <- purposeId.toFutureUUID
      purpose        <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _              <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      depPayload     <- PurposeUpdateContentConverter.apiToDependency(purposeUpdateContent).toFuture
      updatedPurpose <- purposeManagementService.updatePurpose(bearerToken)(purposeUUID, depPayload)
      r              <- PurposeConverter.dependencyToApi(updatedPurpose).toFuture
    } yield r

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, UpdatePurposeBadRequest(purposeId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(purpose) =>
          updatePurpose200(purpose)
        case Failure(ex) =>
          logger.error("Error updating Purpose {}", purposeId, ex)
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
      uuid        <- id.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(bearerToken)(uuid)
      result      <- PurposeConverter.dependencyToApi(purpose).toFuture
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, GetPurposeBadRequest(id))
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
        case Success(purpose) =>
          getPurpose200(purpose)
        case Failure(ex) =>
          logger.error("Error while retrieving purpose {}", id, ex)
          getPurpose400(defaultProblem)
      }
    }
  }

  override def getPurposes(eserviceId: Option[String], consumerId: Option[String], states: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurposes: ToEntityMarshaller[Purposes],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Retrieving Purposes for EService {}, Consumer {} and States {}", eserviceId, consumerId, states)
    val result: Future[Purposes] = for {
      bearerToken  <- getBearer(contexts).toFuture
      eServiceUUID <- eserviceId.traverse(_.toFutureUUID)
      consumerUUID <- consumerId.traverse(_.toFutureUUID)
      states       <- parseArrayParameters(states).traverse(DepPurposeVersionState.fromValue).toFuture
      purposes     <- purposeManagementService.getPurposes(bearerToken)(eServiceUUID, consumerUUID, states)
      result       <- PurposesConverter.dependencyToApi(purposes).toFuture
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, GetPurposesBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
        case Success(purpose) =>
          getPurposes200(purpose)
        case Failure(ex) =>
          logger.error(
            "Error while retrieving purposes for EService {}, Consumer {} and States {}",
            eserviceId,
            consumerId,
            states,
            ex
          )
          getPurposes400(defaultProblem)
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
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      version <- purpose.versions
        .find(_.id == versionUUID)
        .toFuture(ActivatePurposeVersionNotFound(purposeId, versionId))
      userType <- userType(bearerToken)(userUUID, purpose)
      eService <- catalogManagementService.getEServiceById(bearerToken)(purpose.eserviceId)
      updatedVersion <- purposeVersionActivation.activateOrWaitForApproval(bearerToken)(
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
        case Success(result) =>
          activatePurposeVersion200(result)
        case Failure(ex: ActivatePurposeVersionNotFound) =>
          logger.error("Error while activating Version {} of Purpose {} - {}", versionId, purposeId, ex.getMessage)
          val problem = problemOf(StatusCodes.NotFound, ex)
          activatePurposeVersion404(problem)
        case Failure(ex: AgreementNotFound) =>
          logger.error("Error while activating Version {} of Purpose {} - {}", versionId, purposeId, ex.getMessage)
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex: DescriptorNotFound) =>
          logger.error("Error while activating Version {} of Purpose {} - {}", versionId, purposeId, ex.getMessage)
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex: MissingRiskAnalysis) =>
          logger.error("Error while activating Version {} of Purpose {} - {}", versionId, purposeId, ex.getMessage)
          val problem = problemOf(StatusCodes.BadRequest, ex)
          activatePurposeVersion400(problem)
        case Failure(ex) =>
          logger.error("Error while activating Version {} of Purpose {}", versionId, purposeId, ex)
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
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      userType    <- userType(bearerToken)(userUUID, purpose)
      stateDetails = PurposeManagementDependency.StateChangeDetails(userType)
      response <- purposeManagementService.suspendPurposeVersion(bearerToken)(purposeUUID, versionUUID, stateDetails)
      _ <- authorizationManagementService.updateStateOnClients(bearerToken)(
        purposeId = purposeUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, SuspendPurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(r) =>
          suspendPurposeVersion200(r)
        case Failure(ex) =>
          logger.error("Error while suspending Version {} of Purpose {}", versionId, purposeId, ex)
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
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _           <- assertUserIsAConsumer(bearerToken)(userUUID, purpose.consumerId)
      stateDetails = PurposeManagementDependency.StateChangeDetails(ChangedBy.CONSUMER)
      response <- purposeManagementService.archivePurposeVersion(bearerToken)(purposeUUID, versionUUID, stateDetails)
      _ <- authorizationManagementService.updateStateOnClients(bearerToken)(
        purposeId = purposeUUID,
        state = AuthorizationManagementDependency.ClientComponentState.INACTIVE
      )
    } yield PurposeVersionConverter.dependencyToApi(response)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(r) =>
          archivePurposeVersion200(r)
        case Failure(ex) =>
          logger.error("Error while archiving Version {} of Purpose {}", versionId, purposeId, ex)
          archivePurposeVersion400(defaultProblem)
      }
    }
  }

  // TODO This may not work as expected if the user has an active relationship with
  //      both producer and consumer
  def userType(
    bearerToken: String
  )(userId: UUID, purpose: PurposeManagementDependency.Purpose): Future[PurposeManagementDependency.ChangedBy] =
    assertUserIsAConsumer(bearerToken)(userId, purpose.consumerId)
      .recoverWith(_ => assertUserIsAProducer(bearerToken)(userId, purpose.eserviceId))
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
    bearerToken: String
  )(userId: UUID, eServiceId: UUID): Future[PurposeManagementDependency.ChangedBy] =
    for {
      eService      <- catalogManagementService.getEServiceById(bearerToken)(eServiceId)
      relationships <- partyManagementService.getActiveRelationships(bearerToken)(userId, eService.producerId)
      _             <- Either.cond(relationships.items.nonEmpty, (), UserIsNotTheProducer(userId)).toFuture
    } yield PurposeManagementDependency.ChangedBy.PRODUCER

  def handleApiError(
    defaultProblem: Problem
  )(implicit contexts: Seq[(String, String)]): PartialFunction[Try[_], StandardRoute] = {
    case Failure(err: CatalogApiError[_]) =>
      logger.error("Error received from Catalog Management - {}", err.responseContent)
      val problem = err.responseContent match {
        case Some(body: String) => catalogmanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: PartyApiError[_]) =>
      logger.error("Error received from Party Management - {}", err.responseContent)
      val problem = err.responseContent match {
        case Some(body: String) => partymanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: PurposeApiError[_]) =>
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
    case Failure(err: UserNotAllowed) =>
      logger.error("User is neither a Consumer or a Producer. User {}", err.userId)
      val problem = problemOf(StatusCodes.Forbidden, UserNotAllowedError)
      complete(problem.status, problem)
  }
}
