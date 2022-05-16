package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.authorizationmanagement.client.model.{Client, ClientComponentState}

import java.util.UUID
import scala.concurrent.Future

trait AuthorizationManagementService {

  def updateStateOnClients(purposeId: UUID, state: ClientComponentState)(implicit
    contexts: Seq[(String, String)]
  ): Future[Unit]
  def getClients(purposeId: Option[UUID])(implicit contexts: Seq[(String, String)]): Future[Seq[Client]]
  def removePurposeFromClient(purposeId: UUID, clientId: UUID)(implicit contexts: Seq[(String, String)]): Future[Unit]

}
