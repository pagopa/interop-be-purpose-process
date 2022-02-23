package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.partymanagement.client.model.{Organization, Relationships}

import java.util.UUID
import scala.concurrent.Future

trait PartyManagementService {
  def getOrganizationById(bearerToken: String)(organizationId: UUID): Future[Organization]
  def getActiveRelationships(bearerToken: String)(from: UUID, to: UUID): Future[Relationships]
}
