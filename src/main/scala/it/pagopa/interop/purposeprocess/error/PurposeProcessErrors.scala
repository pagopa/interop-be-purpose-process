package it.pagopa.interop.purposeprocess.error

import it.pagopa.interop.commons.utils.errors.ComponentError

object PurposeProcessErrors {

  object CreatePurposeBadRequest extends ComponentError("0001", s"Error creating purpose")
  final case class GetPurposeBadRequest(purposeId: String)
      extends ComponentError("0002", s"Error retrieving purpose $purposeId")
  object GetPurposesBadRequest extends ComponentError("0003", s"Error retrieving purposes")

  final case class SuspendPurposeBadRequest(purposeId: String, versionId: String)
      extends ComponentError("0004", s"Error suspending version $versionId of purpose $purposeId")
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

  final case class ActivatePurposeBadRequest(purposeId: String, versionId: String)
      extends ComponentError("0012", s"Error activating version $versionId for purpose $purposeId")
  final case class ActivatePurposeVersionNotFound(purposeId: String, versionId: String)
      extends ComponentError("0013", s"Version $versionId of Purpose $purposeId not found")
  final case class AgreementNotFound(eServiceId: String, consumerId: String)
      extends ComponentError("0014", s"No Agreement found for EService $eServiceId and Consumer $consumerId")
  final case class DescriptorNotFound(eServiceId: String, descriptorId: String)
      extends ComponentError("0015", s"Descriptor $descriptorId not found for EService $eServiceId")

  final case class MissingRiskAnalysis(purposeId: String, versionId: String)
      extends ComponentError("0016", s"Version $versionId of Purpose $purposeId must contain a valid risk analysis")
  final case class UpdatePurposeBadRequest(purposeId: String)
      extends ComponentError("0017", s"Error updating Purpose $purposeId")

  final case class UndeletableVersionError(purposeId: String)
      extends ComponentError("0018", s"Purpose $purposeId versions are not drafts or are more than one")
  object DeletePurposeBadRequest extends ComponentError("0019", s"Error deleting purpose")

}
