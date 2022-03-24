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
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class CatalogManagementServiceImpl(invoker: CatalogManagementInvoker, api: CatalogManagementApi)(implicit
  ec: ExecutionContext
) extends CatalogManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def getEServiceById(contexts: Seq[(String, String)])(eServiceId: UUID): Future[EService] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.getEService(correlationId, eServiceId.toString, ip)(BearerToken(bearerToken))
      result <- invoker.invoke(request, s"Retrieving EService $eServiceId")
    } yield result
  }
}
