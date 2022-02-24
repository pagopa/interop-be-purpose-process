package it.pagopa.interop.purposeprocess.error

import cats.data.NonEmptyChain

import java.util.UUID

object InternalErrors {
  final case class UserIsNotTheConsumer(userId: UUID) extends Throwable(s"User $userId is not the Consumer")
  final case class UserIsNotTheProducer(userId: UUID) extends Throwable(s"User $userId is not the Producer")
  final case class UserNotAllowed(userId: UUID)       extends Throwable(s"User $userId not allowed")

  final case class RiskAnalysisValidationFailed(failures: NonEmptyChain[RiskAnalysisValidationError])
      extends Throwable(failures.map(_.message).distinct.iterator.mkString("Reasons: ", ", ", ""))
}
