package it.pagopa.interop.purposeprocess.service.impl
import it.pagopa.interop.purposeprocess.service.AgreementManagementService
import it.pagopa.interop.agreementmanagement.model.agreement._
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.purposeprocess.common.readmodel.ReadModelAgreementQueries

import scala.concurrent.{Future, ExecutionContext}
import java.util.UUID

final object AgreementManagementServiceImpl extends AgreementManagementService {

  override def getAgreements(eserviceId: UUID, consumerId: UUID, agreementStates: Seq[PersistentAgreementState])(
    implicit
    ec: ExecutionContext,
    readModel: ReadModelService
  ): Future[Seq[PersistentAgreement]] =
    getAllAgreements(eserviceId, consumerId, agreementStates)

  private def getAgreements(
    eserviceId: UUID,
    consumerId: UUID,
    agreementStates: Seq[PersistentAgreementState],
    offset: Int,
    limit: Int
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentAgreement]] =
    ReadModelAgreementQueries.getAgreements(eserviceId, consumerId, agreementStates, offset, limit)

  private def getAllAgreements(
    eserviceId: UUID,
    consumerId: UUID,
    agreementStates: Seq[PersistentAgreementState]
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentAgreement]] = {

    def getAgreementsFrom(offset: Int): Future[Seq[PersistentAgreement]] =
      getAgreements(
        eserviceId = eserviceId,
        consumerId = consumerId,
        agreementStates = agreementStates,
        offset = offset,
        limit = 50
      )

    def go(start: Int)(as: Seq[PersistentAgreement]): Future[Seq[PersistentAgreement]] =
      getAgreementsFrom(start).flatMap(esec =>
        if (esec.size < 50) Future.successful(as ++ esec) else go(start + 50)(as ++ esec)
      )

    go(0)(Nil)
  }
}
