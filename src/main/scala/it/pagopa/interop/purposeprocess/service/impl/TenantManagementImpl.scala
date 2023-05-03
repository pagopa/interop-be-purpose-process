package it.pagopa.interop.purposeprocess.service.impl

import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.purposeprocess.service.TenantManagementService
import it.pagopa.interop.tenantmanagement.client.invoker.{ApiInvoker => TenantManagementInvoker, ApiError, BearerToken}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.tenantmanagement.client.api.TenantApi
import it.pagopa.interop.tenantmanagement.client.model.Tenant
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.TenantNotFound
import it.pagopa.interop.commons.utils.withHeaders

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}

final class TenantManagementServiceImpl(invoker: TenantManagementInvoker, api: TenantApi)(implicit ec: ExecutionContext)
    extends TenantManagementService {

  private implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def getTenant(tenantId: UUID)(implicit contexts: Seq[(String, String)]): Future[Tenant] =
    withHeaders { (bearerToken, correlationId, ip) =>
      val request =
        api.getTenant(xCorrelationId = correlationId, tenantId = tenantId, xForwardedFor = ip)(BearerToken(bearerToken))
      invoker
        .invoke(request, s"Retrieving Tenant $tenantId")
        .recoverWith { case err: ApiError[_] if err.code == 404 => Future.failed(TenantNotFound(tenantId)) }
    }

}
