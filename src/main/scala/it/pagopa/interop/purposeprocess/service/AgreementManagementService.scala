package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.agreementmanagement.model.agreement._
import it.pagopa.interop.commons.cqrs.service.ReadModelService

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}

trait AgreementManagementService {
  def getAgreements(eServiceId: UUID, consumerId: UUID, states: Seq[PersistentAgreementState])(implicit
    ec: ExecutionContext,
    readModel: ReadModelService
  ): Future[Seq[PersistentAgreement]]
}

object AgreementManagementService {
  val OPERATIVE_AGREEMENT_STATES: Seq[PersistentAgreementState]       = Seq(Active)
  val CHANGE_ESERVICE_AGREEMENT_STATES: Seq[PersistentAgreementState] = Seq(Active, Suspended)
}
