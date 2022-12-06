package it.pagopa.interop.purposeprocess.error

import cats.data.NonEmptyChain

import java.util.UUID

object InternalErrors {
  final case class OrganizationIsNotTheConsumer(organizationId: UUID)
      extends Throwable(s"Organization $organizationId is not the Consumer")
  final case class OrganizationIsNotTheProducer(organizationId: UUID)
      extends Throwable(s"Organization $organizationId is not the Producer")
  final case class OrganizationNotAllowed(organizationId: UUID)
      extends Throwable(s"Organization $organizationId not allowed")

  final case class RiskAnalysisValidationFailed(failures: NonEmptyChain[RiskAnalysisValidationError])
      extends Throwable(failures.map(_.message).distinct.iterator.mkString("Reasons: ", ", ", ""))
}
