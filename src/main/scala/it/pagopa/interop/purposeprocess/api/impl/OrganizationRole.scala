package it.pagopa.interop.purposeprocess.api.impl

import it.pagopa.interop.purposeprocess.error.InternalErrors.OrganizationNotAllowed

import java.util.UUID
import scala.concurrent.Future

sealed trait OrganizationRole

object OrganizationRole {

  final case object CONSUMER           extends OrganizationRole
  final case object PRODUCER           extends OrganizationRole
  final case object PRODUCER_OF_ITSELF extends OrganizationRole

  def apply(organizationId: UUID, producerId: UUID, consumerId: UUID): Future[OrganizationRole] = {
    if (producerId == consumerId && organizationId == producerId) Future.successful(PRODUCER_OF_ITSELF)
    else if (producerId != consumerId && organizationId == consumerId) Future.successful(CONSUMER)
    else if (producerId != consumerId && organizationId == producerId) Future.successful(PRODUCER)
    else Future.failed(OrganizationNotAllowed(organizationId))
  }
}
