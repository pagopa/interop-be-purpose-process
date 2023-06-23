package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.purposeprocess.service.TenantManagementService
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenant
import it.pagopa.interop.purposeprocess.common.readmodel.ReadModelTenantQueries
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.TenantNotFound
import it.pagopa.interop.commons.utils.TypeConversions._

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}

final object TenantManagementServiceImpl extends TenantManagementService {
  override def getTenantById(
    tenantId: UUID
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[PersistentTenant] = {
    ReadModelTenantQueries.getTenantById(tenantId).flatMap(_.toFuture(TenantNotFound(tenantId)))
  }
}
