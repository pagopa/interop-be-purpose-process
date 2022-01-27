package it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiRequest, BearerToken}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Purpose, PurposeSeed}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  PurposeManagementApi,
  PurposeManagementInvoker,
  PurposeManagementService
}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.Future

final case class PurposeManagementServiceImpl(invoker: PurposeManagementInvoker, api: PurposeManagementApi)
    extends PurposeManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def createPurpose(bearerToken: String)(seed: PurposeSeed): Future[Purpose] = {
    val request: ApiRequest[Purpose] = api.createPurpose(seed)(BearerToken(bearerToken))
    invoker.invoke(request, s"Creating purpose for EService ${seed.eserviceId} and Consumer ${seed.consumerId}")
  }

  override def getPurpose(bearerToken: String)(id: UUID): Future[Purpose] = {
    val request: ApiRequest[Purpose] = api.getPurpose(id)(BearerToken(bearerToken))
    invoker.invoke(request, s"Retrieving purpose $id")
  }
}
