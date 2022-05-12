package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.catalogmanagement.client.invoker.BearerToken
import it.pagopa.interop.catalogmanagement.client.model.EService
import it.pagopa.interop.commons.utils.extractHeaders
import it.pagopa.interop.commons.utils.TypeConversions.EitherOps
import it.pagopa.interop.purposeprocess.service.{
  CatalogManagementApi,
  CatalogManagementInvoker,
  CatalogManagementService
}
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class CatalogManagementServiceImpl(invoker: CatalogManagementInvoker, api: CatalogManagementApi)(implicit
  ec: ExecutionContext
) extends CatalogManagementService {

  implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def getEServiceById(eServiceId: UUID)(implicit contexts: Seq[(String, String)]): Future[EService] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.getEService(xCorrelationId = correlationId, eServiceId.toString, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Retrieving EService $eServiceId")
    } yield result
  }
}
