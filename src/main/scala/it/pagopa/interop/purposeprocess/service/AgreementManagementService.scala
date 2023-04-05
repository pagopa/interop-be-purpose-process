package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.agreementmanagement.client.model.{Agreement, AgreementState}

import java.util.UUID
import scala.concurrent.Future

trait AgreementManagementService {
  def getAgreements(eServiceId: UUID, consumerId: UUID, states: Seq[AgreementState])(implicit
    contexts: Seq[(String, String)]
  ): Future[Seq[Agreement]]
}

object AgreementManagementService {
  val OPERATIVE_AGREEMENT_STATES: Seq[AgreementState] = Seq(AgreementState.ACTIVE)
}
