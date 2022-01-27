package it.pagopa.pdnd.interop.uservice.purposeprocess.server.impl

import akka.actor.CoordinatedShutdown
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.directives.SecurityDirectives
import akka.management.scaladsl.AkkaManagement
import it.pagopa.pdnd.interop.commons.jwt.service.JWTReader
import it.pagopa.pdnd.interop.commons.jwt.service.impl.DefaultJWTReader
import it.pagopa.pdnd.interop.commons.jwt.{JWTConfiguration, KID, PublicKeysHolder, SerializedKey}
import it.pagopa.pdnd.interop.commons.utils.AkkaUtils.PassThroughAuthenticator
import it.pagopa.pdnd.interop.commons.utils.TypeConversions.TryOps
import it.pagopa.pdnd.interop.commons.utils.errors.GenericComponentErrors.ValidationRequestError
import it.pagopa.pdnd.interop.commons.utils.{CORSSupport, OpenapiUtils}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.{
  HealthApiMarshallerImpl,
  HealthServiceApiImpl,
  PurposeApiMarshallerImpl,
  PurposeApiServiceImpl,
  problemOf
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.{HealthApi, PurposeApi}
import it.pagopa.pdnd.interop.uservice.purposeprocess.common.system.{
  ApplicationConfiguration,
  classicActorSystem,
  executionContext
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.server.Controller
import it.pagopa.pdnd.interop.uservice.purposeprocess.service._
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl.{
  CatalogManagementServiceImpl,
  PartyManagementServiceImpl,
  PurposeManagementServiceImpl
}
import kamon.Kamon
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait CatalogManagementDependency {
  private final val catalogManagementInvoker: CatalogManagementInvoker = CatalogManagementInvoker()
  private final val catalogManagementApi: CatalogManagementApi = CatalogManagementApi(
    ApplicationConfiguration.catalogManagementURL
  )

  val catalogManagement: CatalogManagementService =
    CatalogManagementServiceImpl(catalogManagementInvoker, catalogManagementApi)
}

trait PartyManagementDependency {
  private final val partyManagementInvoker: PartyManagementInvoker = PartyManagementInvoker()
  private final val partyManagementApi: PartyManagementApi = PartyManagementApi(
    ApplicationConfiguration.partyManagementURL
  )

  val partyManagement: PartyManagementService =
    PartyManagementServiceImpl(partyManagementInvoker, partyManagementApi)
}

trait PurposeManagementDependency {
  private final val purposeManagementInvoker: PurposeManagementInvoker = PurposeManagementInvoker()
  private final val purposeManagementApi: PurposeManagementApi = PurposeManagementApi(
    ApplicationConfiguration.purposeManagementURL
  )

  val purposeManagement: PurposeManagementService =
    PurposeManagementServiceImpl(purposeManagementInvoker, purposeManagementApi)
}

//shuts down the actor system in case of startup errors
case object StartupErrorShutdown extends CoordinatedShutdown.Reason

object Main
    extends App
    with CORSSupport
    with CatalogManagementDependency
    with PartyManagementDependency
    with PurposeManagementDependency {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val dependenciesLoaded: Future[JWTReader] = for {
    keyset <- JWTConfiguration.jwtReader.loadKeyset().toFuture
    jwtValidator = new DefaultJWTReader with PublicKeysHolder {
      var publicKeyset: Map[KID, SerializedKey] = keyset
    }
  } yield jwtValidator

  dependenciesLoaded.transformWith {
    case Success(jwtValidator) => launchApp(jwtValidator)
    case Failure(ex) =>
      logger.error("Startup error", ex)
      logger.error(ex.getStackTrace.mkString("\n"))
      CoordinatedShutdown(classicActorSystem).run(StartupErrorShutdown)
  }

  private def launchApp(jwtReader: JWTReader): Future[Http.ServerBinding] = {
    Kamon.init()

    val purposeApi: PurposeApi = new PurposeApi(
      PurposeApiServiceImpl(catalogManagement, partyManagement, purposeManagement),
      PurposeApiMarshallerImpl,
      jwtReader.OAuth2JWTValidatorAsContexts
    )

    val healthApi: HealthApi = new HealthApi(
      new HealthServiceApiImpl(),
      HealthApiMarshallerImpl,
      SecurityDirectives.authenticateOAuth2("SecurityRealm", PassThroughAuthenticator)
    )

    locally {
      val _ = AkkaManagement.get(classicActorSystem).start()
    }

    val controller: Controller = new Controller(
      health = healthApi,
      purpose = purposeApi,
      validationExceptionToRoute = Some(report => {
        val error =
          problemOf(
            StatusCodes.BadRequest,
            ValidationRequestError(OpenapiUtils.errorFromRequestValidationReport(report))
          )
        complete(error.status, error)(HealthApiMarshallerImpl.toEntityMarshallerProblem)
      })
    )

    logger.info(s"Started build info = ${buildinfo.BuildInfo.toString}")

    val bindingFuture: Future[Http.ServerBinding] =
      Http().newServerAt("0.0.0.0", ApplicationConfiguration.serverPort).bind(corsHandler(controller.routes))
    bindingFuture
  }
}
