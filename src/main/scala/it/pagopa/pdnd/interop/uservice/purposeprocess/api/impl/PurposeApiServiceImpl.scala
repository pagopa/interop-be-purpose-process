package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.{Route, StandardRoute}
import cats.implicits.toTraverseOps
import com.typesafe.scalalogging.Logger
import it.pagopa.pdnd.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.pdnd.interop.commons.utils.AkkaUtils.getBearer
import it.pagopa.pdnd.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.pdnd.interop.commons.utils.TypeConversions._
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.invoker.{ApiError => CatalogApiError}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.{Problem => CatalogProblem}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.invoker.{ApiError => PartyApiError}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.{Problem => PartyProblem}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  ChangedBy,
  Problem => PurposeProblem,
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
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.PurposeProcessErrors.CreatePurposeBadRequest
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
      clientSeed = PurposeSeedConverter.apiToDependency(purposeSeed)
      purpose <- purposeManagementService.createPurpose(bearerToken)(clientSeed)
    } yield PurposeConverter.dependencyToApi(purpose)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
        case Success(purpose) =>
          createPurpose201(purpose)
        case Failure(ex) =>
          logger.error("Error while creating purpose {}", purposeSeed, ex)
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
    } yield PurposeConverter.dependencyToApi(purpose)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
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
    } yield PurposesConverter.dependencyToApi(purposes)

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
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
        .toFuture(new RuntimeException("User ID not found in context"))
      userUUID <- userId._2.toFutureUUID
      userType <- userType(userUUID, purposeUUID)(bearerToken)
      stateChangeDetails = PurposeManagementDependency.StateChangeDetails(userType)
      _ <- purposeManagementService.suspendPurposeVersion(bearerToken)(purposeUUID, versionUUID, stateChangeDetails)
    } yield ()

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
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
        .toFuture(new RuntimeException("User ID not found in context"))
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

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
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
        .toFuture(new RuntimeException("User ID not found in context"))
      userUUID <- userId._2.toFutureUUID
      purpose  <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _        <- assertUserIsAConsumer(userUUID, purpose.consumerId)(bearerToken)
      stateChangeDetails = PurposeManagementDependency.StateChangeDetails(ChangedBy.CONSUMER)
      _ <- purposeManagementService.archivePurposeVersion(bearerToken)(purposeUUID, versionUUID, stateChangeDetails)
    } yield ()

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
    onComplete(result) {
      handleApiError(defaultProblem) orElse {
        case Success(_) =>
          archivePurposeVersion204
        case Failure(ex) =>
          logger.error("Error while archiving Version {} of Purpose {}", versionId, purposeId, ex)
          archivePurposeVersion400(defaultProblem)
      }
    }
  }

//  def userType(userId: UUID, purposeId: UUID)(bearerToken: String): Future[ChangedBy] =
//    for {
//      purpose <- purposeManagementService.getPurpose(bearerToken)(purposeId)
//      consumerRelationships: Relationships <- partyManagementService.getRelationships(bearerToken)(userId, purpose.consumerId)
//      _ <- if(consumerRelationships.items.isEmpty) {
//        for {
//          eService <- catalogManagementService.getEServiceById(bearerToken)(purpose.eserviceId)
//          producerRelationships: Relationships <- partyManagementService.getRelationships(bearerToken)(userId, eService.producerId)
//          _ <- Either.cond(producerRelationships.items.isEmpty, new RuntimeException("User not allowed"), ())
//        } yield ChangedBy.PRODUCER
//      } else Future.successful(())
//    } yield ChangedBy.CONSUMER

  def userType(userId: UUID, purposeId: UUID)(bearerToken: String): Future[ChangedBy] =
    for {
      purpose <- purposeManagementService.getPurpose(bearerToken)(purposeId)
      result <- assertUserIsAConsumer(userId, purpose.consumerId)(bearerToken)
        .map(_ => ChangedBy.CONSUMER)
        .recoverWith[ChangedBy](_ =>
          assertUserIsAProducer(userId, purpose.eserviceId)(bearerToken).map(_ => ChangedBy.PRODUCER)
        )
        .recoverWith[ChangedBy](_ => Future.failed(new RuntimeException("User not allowed")))
    } yield result

  def assertUserIsAConsumer(userId: UUID, consumerId: UUID)(bearerToken: String): Future[Unit] =
    for {
      relationships <- partyManagementService.getActiveRelationships(bearerToken)(userId, consumerId)
      _             <- Either.cond(relationships.items.nonEmpty, (), new RuntimeException("User not a consumer")).toFuture
    } yield ()

  def assertUserIsAProducer(userId: UUID, eServiceId: UUID)(bearerToken: String): Future[Unit] =
    for {
      eService      <- catalogManagementService.getEServiceById(bearerToken)(eServiceId)
      relationships <- partyManagementService.getActiveRelationships(bearerToken)(userId, eService.producerId)
      _             <- Either.cond(relationships.items.nonEmpty, (), new RuntimeException("User not a producer")).toFuture
    } yield ()

  def handleApiError(defaultProblem: Problem): PartialFunction[Try[_], StandardRoute] = {
    case Failure(err: CatalogApiError[_]) =>
      val problem = err.responseContent.fold(defaultProblem) {
        case problem: CatalogProblem => catalogmanagement.ProblemConverter.dependencyToApi(problem)
        case _                       => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: PartyApiError[_]) =>
      val problem = err.responseContent.fold(defaultProblem) {
        case problem: PartyProblem => partymanagement.ProblemConverter.dependencyToApi(problem)
        case _                     => defaultProblem
      }
      complete(problem.status, problem)
    case Failure(err: PurposeApiError[_]) =>
      val problem = err.responseContent.fold(defaultProblem) {
        case problem: PurposeProblem => purposemanagement.ProblemConverter.dependencyToApi(problem)
        case _                       => defaultProblem
      }
      complete(problem.status, problem)

  }
}
