package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.agreementmanagement.client.model.Agreement

import java.util.UUID
import scala.concurrent.Future

trait AgreementManagementService {
  def getAgreements(eServiceId: UUID, consumerId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Seq[Agreement]]
}
