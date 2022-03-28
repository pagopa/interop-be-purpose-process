package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.catalogmanagement.client.model.EService

import java.util.UUID
import scala.concurrent.Future

trait CatalogManagementService {
  def getEServiceById(contexts: Seq[(String, String)])(eServiceId: UUID): Future[EService]
}
