package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.agreementmanagement.client.invoker.BearerToken
import it.pagopa.interop.agreementmanagement.client.model.Agreement
import it.pagopa.interop.commons.utils.TypeConversions.EitherOps
import it.pagopa.interop.commons.utils.extractHeaders
import it.pagopa.interop.purposeprocess.service.{
  AgreementManagementApi,
  AgreementManagementInvoker,
  AgreementManagementService
}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class AgreementManagementServiceImpl(invoker: AgreementManagementInvoker, api: AgreementManagementApi)(
  implicit ec: ExecutionContext
) extends AgreementManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def getAgreements(
    contexts: Seq[(String, String)]
  )(eServiceId: UUID, consumerId: UUID): Future[Seq[Agreement]] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.getAgreements(
        correlationId,
        ip,
        consumerId = Some(consumerId.toString),
        eserviceId = Some(eServiceId.toString),
        state = None
      )(BearerToken(bearerToken))
      result <- invoker.invoke(request, s"Retrieving Agreements for Consumer $consumerId, EService $eServiceId")
    } yield result
  }
}
