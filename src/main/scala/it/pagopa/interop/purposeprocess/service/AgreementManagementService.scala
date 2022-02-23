package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.agreementmanagement.client.model.Agreement

import java.util.UUID
import scala.concurrent.Future

trait AgreementManagementService {
  def getAgreements(bearerToken: String)(eServiceId: UUID, consumerId: UUID): Future[Seq[Agreement]]
}
