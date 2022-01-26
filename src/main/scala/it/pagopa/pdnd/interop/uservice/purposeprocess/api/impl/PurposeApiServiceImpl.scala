package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.Logger
import it.pagopa.pdnd.interop.commons.jwt.service.JWTReader
import it.pagopa.pdnd.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.ApiError
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement.{
  PurposeConverter,
  PurposeSeedConverter
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.PurposeProcessErrors.CreatePurposeBadRequest
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, Purpose, PurposeSeed}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  CatalogManagementService,
  PartyManagementService,
  PurposeManagementService
}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

final case class PurposeApiServiceImpl(
  catalogManagementService: CatalogManagementService,
  partyManagementService: PartyManagementService,
  purposeManagementService: PurposeManagementService,
  jwtReader: JWTReader
)(implicit ec: ExecutionContext)
    extends PurposeApiService {
  private val logger = Logger.takingImplicit[ContextFieldsToLog](LoggerFactory.getLogger(this.getClass))

  override def createPurpose(purposeSeed: PurposeSeed)(implicit
    contexts: Seq[(String, String)],
    toEntityMarshallerPurpose: ToEntityMarshaller[Purpose],
    toEntityMarshallerProblem: ToEntityMarshaller[Problem]
  ): Route = {
    // TODO Not sure about payload logging. What if tomorrow it will contain sensitive data?
    logger.info("Creating Purpose {}", purposeSeed)
    val result: Future[Purpose] = for {
      bearerToken <- validateBearer(contexts, jwtReader)
      _           <- catalogManagementService.getEServiceById(bearerToken)(purposeSeed.eserviceId)
      _           <- partyManagementService.getOrganizationById(bearerToken)(purposeSeed.consumerId)
      clientSeed = PurposeSeedConverter.apiToDependency(purposeSeed)
      purpose <- purposeManagementService.createPurpose(bearerToken)(clientSeed)
    } yield PurposeConverter.dependencyToApi(purpose)

    onComplete(result) {
      case Success(purpose) =>
        createPurpose201(purpose)
      case Failure(ex: ApiError[_]) =>
        logger.error("Error while creating purpose {}", purposeSeed, ex)
        val defaultProblem = problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
        val errorResponse = ex.responseContent.fold(defaultProblem) {
          case err: Problem => err
          case _            => defaultProblem
        }
        complete(errorResponse.status, errorResponse)
      case Failure(ex) =>
        logger.error("Error while creating purpose {}", purposeSeed, ex)
        val errorResponse: Problem =
          problemOf(StatusCodes.BadRequest, CreatePurposeBadRequest)
        createPurpose400(errorResponse)
    }
  }
}
