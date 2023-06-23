package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.authorizationmanagement.client.model.ClientComponentState
import it.pagopa.interop.authorizationmanagement.model.client.PersistentClient
import it.pagopa.interop.commons.cqrs.service.ReadModelService

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}

trait AuthorizationManagementService {

  def updateStateOnClients(purposeId: UUID, versionId: UUID, state: ClientComponentState)(implicit
    contexts: Seq[(String, String)]
  ): Future[Unit]
  def getClients(
    purposeId: UUID
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentClient]]
  def removePurposeFromClient(purposeId: UUID, clientId: UUID)(implicit contexts: Seq[(String, String)]): Future[Unit]

}
