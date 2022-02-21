package it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl

import it.pagopa.interop.authorizationmanagement.client.invoker.{ApiRequest, BearerToken}
import it.pagopa.interop.authorizationmanagement.client.model.{ClientComponentState, ClientPurposeDetailsUpdate}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  AuthorizationManagementInvoker,
  AuthorizationManagementPurposeApi,
  AuthorizationManagementService
}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class AuthorizationManagementServiceImpl(
  invoker: AuthorizationManagementInvoker,
  api: AuthorizationManagementPurposeApi
)(implicit ec: ExecutionContext)
    extends AuthorizationManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def updateStateOnClients(bearerToken: String)(purposeId: UUID, state: ClientComponentState): Future[Unit] = {
    val payload: ClientPurposeDetailsUpdate = ClientPurposeDetailsUpdate(state = state)
    val request: ApiRequest[Unit] =
      api.updatePurposeState(purposeId = purposeId, clientPurposeDetailsUpdate = payload)(BearerToken(bearerToken))
    invoker
      .invoke(request, s"Update Purpose state on all clients")
      .recoverWith { case _ =>
        Future.successful(())
      } // Do not fail because this service should not be blocked by this update
  }
}
