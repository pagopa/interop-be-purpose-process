package it.pagopa.interop.purposeprocess.server.impl

import cats.syntax.all._
import akka.http.scaladsl.Http
import akka.management.scaladsl.AkkaManagement
import it.pagopa.interop.commons.utils.CORSSupport

import it.pagopa.interop.purposeprocess.server.Controller
import it.pagopa.interop.commons.logging.renderBuildInfo

import com.typesafe.scalalogging.Logger

import scala.concurrent.Future
import scala.util.{Failure, Success}
import akka.actor.typed.ActorSystem
import scala.concurrent.ExecutionContext
import akka.actor.typed.scaladsl.Behaviors
import buildinfo.BuildInfo
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import akka.actor.typed.DispatcherSelector
import scala.concurrent.ExecutionContextExecutor

object Main extends App with CORSSupport with Dependencies {

  private val logger: Logger = Logger(this.getClass)

  ActorSystem[Nothing](
    Behaviors.setup[Nothing] { context =>
      implicit val actorSystem: ActorSystem[_]        = context.system
      implicit val executionContext: ExecutionContext = actorSystem.executionContext

      // TODO Remember to modify ApiInvoker to use a configurable EC in the services that depend on this
      val selector: DispatcherSelector         = DispatcherSelector.fromConfig("futures-dispatcher")
      val blockingEc: ExecutionContextExecutor = actorSystem.dispatchers.lookup(selector)

      AkkaManagement.get(actorSystem.classicSystem).start()

      logger.info(renderBuildInfo(BuildInfo))

      val serverBinding: Future[Http.ServerBinding] = for {
        jwtReader <- jwtValidator()
        fManager   = getFileManager(blockingEc)
        controller = new Controller(
          healthApi,
          purposeApi(jwtReader, fManager, blockingEc),
          validationExceptionToRoute.some
        )(actorSystem.classicSystem)
        binding <- Http()
          .newServerAt("0.0.0.0", ApplicationConfiguration.serverPort)
          .bind(corsHandler(controller.routes))
      } yield binding

      serverBinding.onComplete {
        case Success(b) =>
          logger.info(s"Started server at ${b.localAddress.getHostString()}:${b.localAddress.getPort()}")
        case Failure(e) =>
          actorSystem.terminate()
          logger.error("Startup error: ", e)
      }

      Behaviors.empty[Nothing]
    },
    BuildInfo.name
  )
}
