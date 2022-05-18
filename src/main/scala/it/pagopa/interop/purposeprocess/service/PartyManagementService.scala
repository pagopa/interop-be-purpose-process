package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.selfcare.partymanagement.client.model.{Institution, Relationships}

import java.util.UUID
import scala.concurrent.Future

trait PartyManagementService {
  def getInstitutionById(institutionId: UUID)(implicit contexts: Seq[(String, String)]): Future[Institution]
  def getActiveRelationships(from: UUID, to: UUID)(implicit contexts: Seq[(String, String)]): Future[Relationships]

}
