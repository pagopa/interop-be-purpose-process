package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.agreementmanagement.client.invoker.{ApiRequest, BearerToken}
import it.pagopa.interop.agreementmanagement.client.model.Agreement
import it.pagopa.interop.purposeprocess.service.{
  AgreementManagementApi,
  AgreementManagementInvoker,
  AgreementManagementService
}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.Future

final case class AgreementManagementServiceImpl(invoker: AgreementManagementInvoker, api: AgreementManagementApi)
    extends AgreementManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def getAgreements(bearerToken: String)(eServiceId: UUID, consumerId: UUID): Future[Seq[Agreement]] = {
    val request: ApiRequest[Seq[Agreement]] =
      api.getAgreements(consumerId = Some(consumerId.toString), eserviceId = Some(eServiceId.toString), state = None)(
        BearerToken(bearerToken)
      )
    invoker.invoke(request, s"Retrieving Agreements for Consumer $consumerId, EService $eServiceId")
  }
}
