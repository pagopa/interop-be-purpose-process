package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.selfcare.partymanagement.client.model.{Institution, Relationships}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

trait PartyManagementService {
  def getInstitutionById(
    selfcareId: String
  )(implicit contexts: Seq[(String, String)], ec: ExecutionContext): Future[Institution]

  def getActiveRelationships(from: UUID, to: String)(implicit
    contexts: Seq[(String, String)],
    ec: ExecutionContext
  ): Future[Relationships]

}
