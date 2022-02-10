package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.agreementmanagement.client.model.Agreement

import java.util.UUID
import scala.concurrent.Future

trait AgreementManagementService {
  def getAgreements(bearerToken: String)(eServiceId: UUID, consumerId: UUID): Future[Seq[Agreement]]
}
