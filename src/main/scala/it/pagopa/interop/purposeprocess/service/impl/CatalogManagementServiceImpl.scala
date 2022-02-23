package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.catalogmanagement.client.invoker.{ApiRequest, BearerToken}
import it.pagopa.interop.catalogmanagement.client.model.EService
import it.pagopa.interop.purposeprocess.service.{
  CatalogManagementApi,
  CatalogManagementInvoker,
  CatalogManagementService
}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.Future

final case class CatalogManagementServiceImpl(invoker: CatalogManagementInvoker, api: CatalogManagementApi)
    extends CatalogManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def getEServiceById(bearerToken: String)(eServiceId: UUID): Future[EService] = {
    val request: ApiRequest[EService] = api.getEService(eServiceId.toString)(BearerToken(bearerToken))
    invoker.invoke(request, s"Retrieving EService $eServiceId")
  }
}
