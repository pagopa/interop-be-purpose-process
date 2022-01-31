package it.pagopa.pdnd.interop.uservice.purposeprocess.error

import java.util.UUID

object InternalErrors {
  final object UserIdNotInContext                     extends Throwable(s"User Id not in context")
  final case class UserIsNotTheConsumer(userId: UUID) extends Throwable(s"User $userId is not the Consumer")
  final case class UserIsNotTheProducer(userId: UUID) extends Throwable(s"User $userId is not the Producer")
  final case class UserNotAllowed(userId: UUID)       extends Throwable(s"User $userId not allowed")

}
