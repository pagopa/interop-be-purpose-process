package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.authorizationmanagement.client.model.{Client, ClientComponentState}

import java.util.UUID
import scala.concurrent.Future

trait AuthorizationManagementService {

  def updateStateOnClients(contexts: Seq[(String, String)])(purposeId: UUID, state: ClientComponentState): Future[Unit]
  def getClients(contexts: Seq[(String, String)])(purposeId: Option[UUID]): Future[Seq[Client]]
  def removePurposeFromClient(contexts: Seq[(String, String)])(purposeId: UUID, clientId: UUID): Future[Unit]

}
