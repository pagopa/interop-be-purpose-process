package it.pagopa.interop.purposeprocess.common.readmodel

import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.agreementmanagement.model.persistence.JsonFormats._
import it.pagopa.interop.agreementmanagement.model.agreement.{PersistentAgreement, PersistentAgreementState}
import org.mongodb.scala.model.Filters
import org.bson.conversions.Bson

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

object ReadModelAgreementQueries extends ReadModelQuery {

  private def getAgreementsFilters(eServiceId: UUID, consumerId: UUID, states: Seq[PersistentAgreementState]): Bson = {
    val eserviceIdFilter      = Filters.eq("data.eserviceId", eServiceId.toString)
    val consumerIdFilter      = Filters.eq("data.consumerId", consumerId.toString)
    val agreementStatesFilter = mapToVarArgs(
      states
        .map(_.toString)
        .map(Filters.eq("data.state", _))
    )(Filters.or)

    mapToVarArgs(Seq(eserviceIdFilter, consumerIdFilter) ++ agreementStatesFilter.toList)(Filters.and)
      .getOrElse(Filters.empty())
  }

  def getAgreements(eServiceId: UUID, consumerId: UUID, states: Seq[PersistentAgreementState], offset: Int, limit: Int)(
    implicit
    ec: ExecutionContext,
    readModel: ReadModelService
  ): Future[Seq[PersistentAgreement]] = {
    val filters: Bson = getAgreementsFilters(eServiceId = eServiceId, consumerId = consumerId, states = states)
    readModel.find[PersistentAgreement]("agreements", filters, offset, limit)
  }
}
