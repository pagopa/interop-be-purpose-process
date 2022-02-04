package it.pagopa.pdnd.interop.uservice.purposeprocess.error

import it.pagopa.pdnd.interop.commons.utils.errors.ComponentError

object PurposeProcessErrors {

  object CreatePurposeBadRequest extends ComponentError("0001", s"Error creating purpose")
  final case class GetPurposeBadRequest(purposeId: String)
      extends ComponentError("0002", s"Error retrieving purpose $purposeId")
  object GetPurposesBadRequest extends ComponentError("0003", s"Error retrieving purposes")

  final case class SuspendPurposeBadRequest(purposeId: String, versionId: String)
      extends ComponentError("0004", s"Error suspending version $versionId of purpose $purposeId")
  final case class WaitForApprovalPurposeBadRequest(purposeId: String, versionId: String)
      extends ComponentError("0005", s"Error waiting for approval for version $versionId of purpose $purposeId")
  final case class ArchivePurposeBadRequest(purposeId: String, versionId: String)
      extends ComponentError("0006", s"Error archiving version $versionId of purpose $purposeId")

  object OnlyConsumerAllowedError
      extends ComponentError("0007", s"Only the Consumer is allowed to perform this operation")
  object OnlyProducerAllowedError
      extends ComponentError("0008", s"Only the Producer is allowed to perform this operation")
  object UserNotAllowedError extends ComponentError("0009", s"User is not allowed to perform this operation")

  final case class RiskAnalysisFormError(errorMessage: String)
      extends ComponentError("0010", s"Risk analysis validation failed. $errorMessage")

  final case class CreatePurposeVersionBadRequest(purposeId: String)
      extends ComponentError("0011", s"Error creating version for purpose $purposeId")

}
