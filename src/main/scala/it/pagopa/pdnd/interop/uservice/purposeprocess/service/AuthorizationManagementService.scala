package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.keymanagement.client.model.ClientComponentState

import java.util.UUID
import scala.concurrent.Future

trait AuthorizationManagementService {

  def updateStateOnClients(bearerToken: String)(purposeId: UUID, state: ClientComponentState): Future[Unit]

}
