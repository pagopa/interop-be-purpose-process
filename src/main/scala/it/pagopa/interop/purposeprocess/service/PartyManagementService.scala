package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.partymanagement.client.model.{Institution, Relationships}

import java.util.UUID
import scala.concurrent.Future

trait PartyManagementService {
  def getInstitutionById(bearerToken: String)(institutionId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Institution]
  def getActiveRelationships(bearerToken: String)(from: UUID, to: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Relationships]
}
