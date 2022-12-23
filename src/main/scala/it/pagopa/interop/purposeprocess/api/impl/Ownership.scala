package it.pagopa.interop.purposeprocess.api.impl

import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.ChangedBy
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.OrganizationNotAllowed

import java.util.UUID

sealed trait Ownership {
  def toChangedBy: ChangedBy = this match {
    case Ownership.CONSUMER      => ChangedBy.CONSUMER
    case Ownership.PRODUCER      => ChangedBy.PRODUCER
    case Ownership.SELF_CONSUMER => ChangedBy.PRODUCER
  }
}

object Ownership {

  final case object CONSUMER      extends Ownership
  final case object PRODUCER      extends Ownership
  final case object SELF_CONSUMER extends Ownership

  def getOrganizationRole(
    organizationId: UUID,
    producerId: UUID,
    consumerId: UUID
  ): Either[OrganizationNotAllowed, Ownership] = {
    if (producerId == consumerId && organizationId == producerId) SELF_CONSUMER.asRight
    else if (producerId != consumerId && organizationId == consumerId) CONSUMER.asRight
    else if (producerId != consumerId && organizationId == producerId) PRODUCER.asRight
    else OrganizationNotAllowed(organizationId).asLeft
  }

}
