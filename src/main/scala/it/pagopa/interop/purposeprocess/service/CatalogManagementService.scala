package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.catalogmanagement.model.CatalogItem
import it.pagopa.interop.commons.cqrs.service.ReadModelService

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}

trait CatalogManagementService {
  def getEServiceById(eServiceId: UUID)(implicit ec: ExecutionContext, readModel: ReadModelService): Future[CatalogItem]
}
