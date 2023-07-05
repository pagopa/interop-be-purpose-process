package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.purposeprocess.service.CatalogManagementService
import it.pagopa.interop.catalogmanagement.model.CatalogItem
import it.pagopa.interop.purposeprocess.common.readmodel.ReadModelCatalogQueries
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.EServiceNotFound

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}

final object CatalogManagementServiceImpl extends CatalogManagementService {

  override def getEServiceById(
    eServiceId: UUID
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[CatalogItem] = {
    ReadModelCatalogQueries.getEServiceById(eServiceId).flatMap(_.toFuture(EServiceNotFound(eServiceId)))
  }
}
