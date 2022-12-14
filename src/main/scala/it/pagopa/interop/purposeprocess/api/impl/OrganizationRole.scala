package it.pagopa.interop.purposeprocess.api.impl

import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.ChangedBy
import it.pagopa.interop.purposeprocess.error.InternalErrors.OrganizationNotAllowed

import java.util.UUID

sealed trait OrganizationRole {
  def toChangedBy: ChangedBy = this match {
    case OrganizationRole.CONSUMER      => ChangedBy.CONSUMER
    case OrganizationRole.PRODUCER      => ChangedBy.PRODUCER
    case OrganizationRole.SELF_CONSUMER => ChangedBy.PRODUCER
  }
}

object OrganizationRole {

  final case object CONSUMER      extends OrganizationRole
  final case object PRODUCER      extends OrganizationRole
  final case object SELF_CONSUMER extends OrganizationRole

  def getOrganizationRole(
    organizationId: UUID,
    producerId: UUID,
    consumerId: UUID
  ): Either[OrganizationNotAllowed, OrganizationRole] = {
    if (producerId == consumerId && organizationId == producerId) SELF_CONSUMER.asRight
    else if (producerId != consumerId && organizationId == consumerId) CONSUMER.asRight
    else if (producerId != consumerId && organizationId == producerId) PRODUCER.asRight
    else OrganizationNotAllowed(organizationId).asLeft
  }

}
