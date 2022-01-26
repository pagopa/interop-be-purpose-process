package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.EService

import java.util.UUID
import scala.concurrent.Future

trait CatalogManagementService {
  def getEServiceById(bearerToken: String)(eServiceId: UUID): Future[EService]
}
