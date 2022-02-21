package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.interop.authorizationmanagement.client.model.{Client, ClientComponentState}

import java.util.UUID
import scala.concurrent.Future

trait AuthorizationManagementService {

  def updateStateOnClients(bearerToken: String)(purposeId: UUID, state: ClientComponentState): Future[Unit]
  def getClients(bearerToken: String)(purposeId: Option[UUID]): Future[Seq[Client]]

}
