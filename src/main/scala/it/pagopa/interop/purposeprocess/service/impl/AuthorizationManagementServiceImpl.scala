package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.authorizationmanagement.client.invoker.BearerToken
import it.pagopa.interop.authorizationmanagement.client.model.{Client, ClientComponentState, ClientPurposeDetailsUpdate}
import it.pagopa.interop.commons.utils.extractHeaders
import it.pagopa.interop.commons.utils.TypeConversions.EitherOps
import it.pagopa.interop.purposeprocess.service._
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class AuthorizationManagementServiceImpl(
  invoker: AuthorizationManagementInvoker,
  purposeApi: AuthorizationManagementPurposeApi,
  clientApi: AuthorizationManagementClientApi
)(implicit ec: ExecutionContext)
    extends AuthorizationManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def updateStateOnClients(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, state: ClientComponentState): Future[Unit] = {
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

  override def getClients(contexts: Seq[(String, String)])(purposeId: Option[UUID]): Future[Seq[Client]] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = clientApi.listClients(xCorrelationId = correlationId, xForwardedFor = ip, purposeId = purposeId)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Retrieving Clients by Purpose Id $purposeId")
    } yield result

  }

  override def removePurposeFromClient(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, clientId: UUID): Future[Unit] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = purposeApi.removeClientPurpose(xCorrelationId = correlationId, clientId, purposeId, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Removing purpose $purposeId from client $clientId")
    } yield result
  }
}
