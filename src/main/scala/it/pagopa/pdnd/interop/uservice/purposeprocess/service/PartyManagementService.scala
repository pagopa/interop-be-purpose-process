package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.{Organization, Relationships}

import java.util.UUID
import scala.concurrent.Future

trait PartyManagementService {
  def getOrganizationById(bearerToken: String)(organizationId: UUID): Future[Organization]
  def getActiveRelationships(bearerToken: String)(from: UUID, to: UUID): Future[Relationships]
}
