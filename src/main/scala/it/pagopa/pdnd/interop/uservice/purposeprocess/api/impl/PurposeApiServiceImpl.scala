package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.{MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.directives.FileInfo
import akka.http.scaladsl.server.{Route, StandardRoute}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import it.pagopa.pdnd.interop.commons.files.service.{FileManager, StorageFilePath}
import it.pagopa.pdnd.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.pdnd.interop.commons.utils.AkkaUtils.{getBearer, getUidFuture}
import it.pagopa.pdnd.interop.commons.utils.OpenapiUtils.parseArrayParameters
import it.pagopa.pdnd.interop.commons.utils.TypeConversions._
import it.pagopa.pdnd.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.invoker.{ApiError => CatalogApiError}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.{model => CatalogManagementDependency}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.invoker.{ApiError => PartyApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  ChangedBy,
  PurposeVersionState => DepPurposeVersionState
}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement._
import it.pagopa.pdnd.interop.uservice.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.InternalErrors.{
  RiskAnalysisValidationFailed,
  UserIsNotTheConsumer,
  UserIsNotTheProducer,
  UserNotAllowed
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.PurposeProcessErrors._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  CatalogManagementService,
  PDFCreator,
  PartyManagementService,
  PurposeManagementService
}
import org.slf4j.LoggerFactory

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

final case class PurposeApiServiceImpl(
  catalogManagementService: CatalogManagementService,
  partyManagementService: PartyManagementService,
  purposeManagementService: PurposeManagementService,
  fileManager: FileManager,
  pdfCreator: PDFCreator,
  UUIDSupplier: UUIDSupplier,
  dateTimeSupplier: OffsetDateTimeSupplier
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
      _           <- assertUserIsAConsumer(userUUID, purpose.consumerId)(bearerToken)
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
    toEntityMarshallerProblem: ToEntityMarshaller[Problem],
    toEntityMarshallerActivatePurposeVersionResult: ToEntityMarshaller[ActivatePurposeVersionResult]
  ): Route = {
    logger.info("Activating Version {} of Purpose {}", versionId, purposeId)
    val result: Future[PurposeManagementDependency.PurposeVersionState] = for {
      bearerToken <- getBearer(contexts).toFuture
      purposeUUID <- purposeId.toFutureUUID
      versionUUID <- versionId.toFutureUUID
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      version     <- purpose.versions.find(_.id == versionUUID).toFuture(new RuntimeException("Version not found"))
      userType    <- userType(userUUID, purpose)(bearerToken)
      eService    <- catalogManagementService.getEServiceById(bearerToken)(purposeUUID)
      newState    <- activateOrWaitForApproval(bearerToken)(eService, purpose, version, userType, userUUID)
    } yield newState

    val defaultProblem: Problem = problemOf(StatusCodes.BadRequest, ActivatePurposeBadRequest(purposeId, versionId))
    onComplete(result) {
      handleApiError(defaultProblem) orElse handleUserTypeError orElse {
        case Success(result) =>
          activatePurposeVersion200(
            ActivatePurposeVersionResult(newState = PurposeVersionStateConverter.dependencyToApi(result))
          )
        case Failure(ex) =>
          logger.error("Error while activating Version {} of Purpose {}", versionId, purposeId, ex)
          activatePurposeVersion400(defaultProblem)
      }
    }
  }

  // TODO Rename
  def activateOrWaitForApproval(bearerToken: String)(
    eService: CatalogManagementDependency.EService,
    purpose: PurposeManagementDependency.Purpose,
    version: PurposeManagementDependency.PurposeVersion,
    userType: PurposeManagementDependency.ChangedBy,
    userId: UUID
  ): Future[PurposeManagementDependency.PurposeVersionState] = {
    import PurposeManagementDependency.ChangedBy._
    import PurposeManagementDependency.PurposeVersionState._
    import PurposeManagementDependency.{ActivatePurposeVersionPayload, PurposeVersionState, StateChangeDetails}

    val changeDetails = StateChangeDetails(changedBy = userType)

    def waitForApproval(): Future[PurposeVersionState] =
      purposeManagementService
        .waitForApprovalPurposeVersion(bearerToken)(purpose.id, version.id, changeDetails)
        .as(WAITING_FOR_APPROVAL)
    def activate(): Future[PurposeVersionState] = {
      val payload =
        ActivatePurposeVersionPayload(riskAnalysis = version.riskAnalysis, stateChangeDetails = changeDetails)
      purposeManagementService
        .activatePurposeVersion(bearerToken)(purpose.id, version.id, payload)
        .as(ACTIVE)
    }

    (version.state, userType) match {
      case (DRAFT, CONSUMER) =>
        for {
          isAllowed <- isLoadAllowed(bearerToken)(eService, purpose)
          result <-
            if (isAllowed) firstVersionActivation(bearerToken)(purpose, version, changeDetails).as(ACTIVE)
            else waitForApproval()
        } yield result
      case (DRAFT, PRODUCER) => Future.failed(UserIsNotTheConsumer(userId))

      case (WAITING_FOR_APPROVAL, CONSUMER) => Future.failed(UserIsNotTheProducer(userId))
      case (WAITING_FOR_APPROVAL, PRODUCER) =>
        firstVersionActivation(bearerToken)(purpose, version, changeDetails).as(ACTIVE)

      case (SUSPENDED, CONSUMER) =>
        for {
          isAllowed <- isLoadAllowed(bearerToken)(eService, purpose)
          result <-
            if (isAllowed) activate()
            else waitForApproval()
        } yield result
      case (SUSPENDED, PRODUCER) => activate() // TODO Can we consider this active?

      case _ => Future.failed(UserNotAllowed(userId))
    }
  }

  // TODO Rename
  def isLoadAllowed(
    bearerToken: String
  )(eService: CatalogManagementDependency.EService, purpose: PurposeManagementDependency.Purpose): Future[Boolean] = {
    for {
      purposes <- purposeManagementService.getPurposes(bearerToken)(
        eserviceId = Some(purpose.eserviceId),
        consumerId = Some(purpose.consumerId),
        states = Seq(PurposeManagementDependency.PurposeVersionState.ACTIVE)
      )
      activeVersions = purposes.purposes.flatMap(
        _.versions.filter(_.state == PurposeManagementDependency.PurposeVersionState.ACTIVE)
      )
      loadRequestsSum = activeVersions.map(_.dailyCalls).sum
      _               = eService // TODO Delete me
//    } yield eService.dailyCalls <= loadRequestsSum // TODO Uncomment
    } yield 100 <= loadRequestsSum

  }

  def firstVersionActivation(bearerToken: String)(
    purpose: PurposeManagementDependency.Purpose,
    version: PurposeManagementDependency.PurposeVersion,
    stateChangeDetails: PurposeManagementDependency.StateChangeDetails
  ): Future[Unit] = {
    val documentId: UUID = UUIDSupplier.get
    for {
      path <- createRiskAnalysisDocument(documentId, purpose, version)
      payload = PurposeManagementDependency.ActivatePurposeVersionPayload(
        riskAnalysis = Some(
          PurposeManagementDependency.PurposeVersionDocument(
            id = documentId,
            contentType = MediaTypes.`application/pdf`.toString(),
            path = path,
            createdAt = dateTimeSupplier.get
          )
        ),
        stateChangeDetails = stateChangeDetails
      )
      _ <- purposeManagementService.activatePurposeVersion(bearerToken)(purpose.id, version.id, payload)
    } yield ()
  }

  def createRiskAnalysisDocument(
    documentId: UUID,
    purpose: PurposeManagementDependency.Purpose,
    version: PurposeManagementDependency.PurposeVersion
  ): Future[StorageFilePath] = {
    val template = Source
      .fromResource("riskAnalysisTemplate.html")
      .getLines()
      .mkString(System.lineSeparator()) // TODO This can be loaded on startup
    for {
      document <- pdfCreator.createDocument(template, purpose.riskAnalysisForm, version.dailyCalls)
      fileInfo = FileInfo("riskAnalysisDocument", document.getName, MediaTypes.`application/pdf`)
      path <- fileManager.store(ApplicationConfiguration.storageContainer)(documentId, (fileInfo, document))
    } yield path
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
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      userType    <- userType(userUUID, purpose)(bearerToken)
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
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _           <- assertUserIsAConsumer(userUUID, purpose.consumerId)(bearerToken)
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
      userId      <- getUidFuture(contexts)
      userUUID    <- userId.toFutureUUID
      purpose     <- purposeManagementService.getPurpose(bearerToken)(purposeUUID)
      _           <- assertUserIsAConsumer(userUUID, purpose.consumerId)(bearerToken)
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

  def handleUserTypeError[T]: PartialFunction[Try[T], StandardRoute] = {
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
