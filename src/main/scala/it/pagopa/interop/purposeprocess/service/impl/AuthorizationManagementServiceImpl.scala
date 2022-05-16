package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.authorizationmanagement.client.invoker.BearerToken
import it.pagopa.interop.authorizationmanagement.client.model.{Client, ClientComponentState, ClientPurposeDetailsUpdate}
import it.pagopa.interop.commons.utils.extractHeaders
import it.pagopa.interop.commons.utils.TypeConversions.EitherOps
import it.pagopa.interop.purposeprocess.service._
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class AuthorizationManagementServiceImpl(
  invoker: AuthorizationManagementInvoker,
  purposeApi: AuthorizationManagementPurposeApi,
  clientApi: AuthorizationManagementClientApi
)(implicit ec: ExecutionContext)
    extends AuthorizationManagementService {

  implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def updateStateOnClients(purposeId: UUID, state: ClientComponentState)(implicit
    contexts: Seq[(String, String)]
  ): Future[Unit] = {
    val payload: ClientPurposeDetailsUpdate = ClientPurposeDetailsUpdate(state = state)

    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = purposeApi.updatePurposeState(
        xCorrelationId = correlationId,
        purposeId = purposeId,
        clientPurposeDetailsUpdate = payload,
        xForwardedFor = ip
      )(BearerToken(bearerToken))
      // Do not fail because this service should not be blocked by this update
      result <- invoker
        .invoke(request, s"Update Purpose state on all clients")
        .recoverWith { case _ =>
          Future.successful(())
        }
    } yield result

  }

  override def getClients(purposeId: Option[UUID])(implicit contexts: Seq[(String, String)]): Future[Seq[Client]] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = clientApi.listClients(xCorrelationId = correlationId, xForwardedFor = ip, purposeId = purposeId)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Retrieving Clients by Purpose Id $purposeId")
    } yield result

  }

  override def removePurposeFromClient(purposeId: UUID, clientId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Unit] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = purposeApi.removeClientPurpose(xCorrelationId = correlationId, clientId, purposeId, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Removing purpose $purposeId from client $clientId")
    } yield result
  }
}
