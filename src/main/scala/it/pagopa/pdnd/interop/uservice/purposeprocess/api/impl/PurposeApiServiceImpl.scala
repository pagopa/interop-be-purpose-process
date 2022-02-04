package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.{Route, StandardRoute}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import it.pagopa.pdnd.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.pdnd.interop.commons.utils.AkkaUtils.getBearer
import it.pagopa.pdnd.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.pdnd.interop.commons.utils.TypeConversions._
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.invoker.{ApiError => CatalogApiError}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.invoker.{ApiError => PartyApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  ChangedBy,
  PurposeVersionState => DepPurposeVersionState
}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement.{
  PurposeConverter,
  PurposeSeedConverter,
  PurposesConverter
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.InternalErrors.{
  RiskAnalysisValidationFailed,
  UserIdNotInContext,
  UserIsNotTheConsumer,
  UserIsNotTheProducer,
  UserNotAllowed
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, Purpose, PurposeSeed, Purposes}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  CatalogManagementService,
  PartyManagementService,
  PurposeManagementService
}
import org.slf4j.LoggerFactory

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

final case class PurposeApiServiceImpl(
  catalogManagementService: CatalogManagementService,
  partyManagementService: PartyManagementService,
  purposeManagementService: PurposeManagementService
)(implicit ec: ExecutionContext)
    extends PurposeApiService {
  private val logger = Logger.takingImplicit[ContextFieldsToLog](LoggerFactory.getLogger(this.getClass))

  override def createPurpose(purposeSeed: PurposeSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Creating Purpose {}", purposeSeed)
    val result: Future[Purpose] = for {
      bearerToken <- getBearer(contexts).toFuture
      _           <- catalogManagementService.getEServiceById(bearerToken)(purposeSeed.eserviceId)
      _           <- partyManagementService.getOrganizationById(bearerToken)(purposeSeed.consumerId)
      clientSeed  <- PurposeSeedConverter.apiToDependency(purposeSeed).toFuture
      purpose     <- purposeManagementService.createPurpose(bearerToken)(clientSeed)
      result      <- PurposeConverter.dependencyToApi(purpose).toFuture
    } yield result

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
        case Success(purpose) =>
          createPurpose201(purpose)
        case Failure(ex: RiskAnalysisValidationFailed) =>
          logger.error("Error creating purpose - Risk Analysis Validation failed {}", purposeSeed, ex)
          val problem = problemOf(StatusCodes.BadRequest, RiskAnalysisFormError(ex.getMessage))
          createPurpose400(problem)
        case Failure(ex) =>
          logger.error("Error creating purpose {}", purposeSeed, ex)
          createPurpose400(defaultProblem)
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

  override def suspendPurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Suspending Version {} of Purpose {}", versionId, purposeId)
    val result: Future[Unit] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId <- contexts
        .find(_._1 == it.pagopa.pdnd.interop.commons.utils.UID)
        .toFuture(UserIdNotInContext)
      userUUID <- userId._2.toFutureUUID
      purpose  <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      userType <- userType(userUUID, purpose)(bearerToken)
      stateChangeDetails = PurposeManagementDependency.StateChangeDetails(userType)
      _ <- purposeManagementService.suspendPurposeVersion(bearerToken)(purposeUUID, versionUUID, stateChangeDetails)
    } yield ()

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, SuspendPurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(_) =>
          suspendPurposeVersion204
        case Failure(ex) =>
          logger.error("Error while suspending Version {} of Purpose {}", versionId, purposeId, ex)
          suspendPurposeVersion400(defaultProblem)
      }
    }
  }

  override def waitForApprovalPurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Wait for Approval for Version {} of Purpose {}", versionId, purposeId)
    val result: Future[Unit] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId <- contexts
        .find(_._1 == it.pagopa.pdnd.interop.commons.utils.UID)
        .toFuture(UserIdNotInContext)
      userUUID <- userId._2.toFutureUUID
      purpose  <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _        <- assertUserIsAConsumer(userUUID, purpose.consumerId)(bearerToken)
      stateChangeDetails = PurposeManagementDependency.StateChangeDetails(ChangedBy.CONSUMER)
      _ <- purposeManagementService.waitForApprovalPurposeVersion(bearerToken)(
        purposeUUID,
        versionUUID,
        stateChangeDetails
      )
    } yield ()

    val defaultProblem: Problem =
      problemOf(StatusCodes.BadRequest, WaitForApprovalPurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(_) =>
          waitForApprovalPurposeVersion204
        case Failure(ex) =>
          logger.error("Error while waiting for approval for Version {} of Purpose {}", versionId, purposeId, ex)
          waitForApprovalPurposeVersion400(defaultProblem)
      }
    }
  }

  override def archivePurposeVersion(purposeId: String, versionId: String)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    logger.info("Archiving for Version {} of Purpose {}", versionId, purposeId)
    val result: Future[Unit] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId <- contexts
        .find(_._1 == it.pagopa.pdnd.interop.commons.utils.UID)
        .toFuture(UserIdNotInContext)
      userUUID <- userId._2.toFutureUUID
      purpose  <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _        <- assertUserIsAConsumer(userUUID, purpose.consumerId)(bearerToken)
      stateChangeDetails = PurposeManagementDependency.StateChangeDetails(ChangedBy.CONSUMER)
      _ <- purposeManagementService.archivePurposeVersion(bearerToken)(purposeUUID, versionUUID, stateChangeDetails)
    } yield ()

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, ArchivePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(_) =>
          archivePurposeVersion204
        case Failure(ex) =>
          logger.error("Error while archiving Version {} of Purpose {}", versionId, purposeId, ex)
          archivePurposeVersion400(defaultProblem)
      }
    }
  }

  // TODO This may not work as expected if the user has an active relationship with
  //      both producer and consumer
  def userType(userId: UUID, purpose: PurposeManagementDependency.Purpose)(
    bearerToken: String
  ): Future[PurposeManagementDependency.ChangedBy] =
    assertUserIsAConsumer(userId, purpose.consumerId)(bearerToken)
      .recoverWith(_ => assertUserIsAProducer(userId, purpose.eserviceId)(bearerToken))
      .recoverWith(_ => Future.failed(UserNotAllowed(userId)))

  // TODO This may not work as expected if the user has an active relationship with
  //      both producer and consumer
  def assertUserIsAConsumer(userId: UUID, consumerId: UUID)(
    bearerToken: String
  ): Future[PurposeManagementDependency.ChangedBy] =
    for {
      relationships <- partyManagementService.getActiveRelationships(bearerToken)(userId, consumerId)
      _             <- Either.cond(relationships.items.nonEmpty, (), UserIsNotTheConsumer(userId)).toFuture
    } yield PurposeManagementDependency.ChangedBy.CONSUMER

  // TODO This may not work as expected if the user has an active relationship with
  //      both producer and consumer
  def assertUserIsAProducer(userId: UUID, eServiceId: UUID)(
    bearerToken: String
  ): Future[PurposeManagementDependency.ChangedBy] =
    for {
      eService      <- catalogManagementService.getEServiceById(bearerToken)(eServiceId)
      relationships <- partyManagementService.getActiveRelationships(bearerToken)(userId, eService.producerId)
      _             <- Either.cond(relationships.items.nonEmpty, (), UserIsNotTheProducer(userId)).toFuture
    } yield PurposeManagementDependency.ChangedBy.PRODUCER

  def handleApiError(defaultProblem: Problem): PartialFunction[Try[_], StandardRoute] = {
    case Failure(err: CatalogApiError[_]) =>
      val problem = err.responseContent match {
        case Some(body: String) => catalogmanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: PartyApiError[_]) =>
      val problem = err.responseContent match {
        case Some(body: String) => partymanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: PurposeApiError[_]) =>
      val problem = err.responseContent match {
        case Some(body: String) => purposemanagement.ProblemConverter.fromString(body).getOrElse(defaultProblem)
        case _                  => defaultProblem
      }
      complete(problem.status, problem)
  }

  val handleUserTypeError: PartialFunction[Try[_], StandardRoute] = {
    case Failure(_: UserIsNotTheConsumer) =>
      val problem = problemOf(StatusCodes.Forbidden, OnlyConsumerAllowedError)
      complete(problem.status, problem)
    case Failure(_: UserIsNotTheProducer) =>
      val problem = problemOf(StatusCodes.Forbidden, OnlyProducerAllowedError)
      complete(problem.status, problem)
    case Failure(_: UserNotAllowed) =>
      val problem = problemOf(StatusCodes.Forbidden, UserNotAllowedError)
      complete(problem.status, problem)
  }
}
