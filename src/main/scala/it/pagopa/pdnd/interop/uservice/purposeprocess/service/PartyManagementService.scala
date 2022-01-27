package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.Organization

import java.util.UUID
import scala.concurrent.Future

trait PartyManagementService {
  def getOrganizationById(bearerToken: String)(organizationId: UUID): Future[Organization]
}
