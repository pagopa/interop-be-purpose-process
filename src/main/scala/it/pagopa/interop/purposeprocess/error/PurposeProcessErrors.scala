package it.pagopa.interop.purposeprocess.error

import cats.data.NonEmptyChain
import it.pagopa.interop.commons.utils.errors.ComponentError
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind

import java.util.UUID
import it.pagopa.interop.commons.riskanalysis.error

object PurposeProcessErrors {

  final case class OrganizationIsNotTheConsumer(organizationId: UUID)
      extends ComponentError("0001", s"Organization $organizationId is not allowed to perform the operation")
  final case class OrganizationIsNotTheProducer(organizationId: UUID)
      extends ComponentError("0002", s"Organization $organizationId is not allowed to perform the operation")
  final case class OrganizationNotAllowed(organizationId: UUID)
      extends ComponentError("0003", s"Organization $organizationId is not allowed to perform the operation")

  final case class RiskAnalysisValidationFailed(reason: String)
      extends ComponentError("0004", s"Risk analysis validation failed. Reasons: $reason")
  object RiskAnalysisValidationFailed {
    def apply(failures: NonEmptyChain[error.RiskAnalysisValidationError]): RiskAnalysisValidationFailed =
      RiskAnalysisValidationFailed(failures.map(_.message).distinct.iterator.mkString("[", ", ", "]"))
  }

  final case class AgreementNotFound(eServiceId: String, consumerId: String)
      extends ComponentError("0005", s"No Agreement found for EService $eServiceId and Consumer $consumerId")
  final case class DescriptorNotFound(eServiceId: String, descriptorId: String)
      extends ComponentError("0006", s"Descriptor $descriptorId not found for EService $eServiceId")

  final case class MissingRiskAnalysis(purposeId: UUID)
      extends ComponentError("0007", s"Purpose $purposeId must contain a valid risk analysis")

  final case class PurposeCannotBeDeleted(purposeId: String)
      extends ComponentError("0008", s"Versions in Purpose $purposeId do not allow deletion")
  final case class PurposeVersionCannotBeDeleted(purposeId: String, versionId: String)
      extends ComponentError("0009", s"Version $versionId of Purpose $purposeId cannot be deleted")

  final case class PurposeVersionNotFound(purposeId: UUID, versionId: UUID)
      extends ComponentError("0010", s"Version $versionId of Purpose $purposeId not found")
  final case class PurposeVersionDocumentNotFound(purposeId: String, versionId: String, documentId: String)
      extends ComponentError("0011", s"Document $documentId of version $versionId of Purpose $purposeId not found")

  final case class PurposeNotFound(purposeId: UUID) extends ComponentError("0012", s"Purpose $purposeId not found")

  final case class PurposeVersionConflict(purposeId: UUID)
      extends ComponentError("0013", s"Version conflict for Purpose $purposeId")

  final case class PurposeCannotBeCloned(purposeId: String)
      extends ComponentError("0014", s"Purpose $purposeId cannot be cloned")

  final case class PurposeNotInDraftState(purposeId: UUID)
      extends ComponentError("0015", s"Purpose $purposeId is not in a DRAFT state")

  final case class PurposeVersionNotInDraftState(purposeId: UUID, versionId: UUID)
      extends ComponentError("0016", s"Version $versionId of Purpose $purposeId is not in a DRAFT state")

  final case class TenantNotFound(tenantId: UUID)
      extends ComponentError("0017", s"Tenant ${tenantId.toString} not found")

  final case class TenantKindNotFound(tenantId: UUID)
      extends ComponentError("0018", s"Tenant kind for tenant ${tenantId.toString} not found")

  final case class RiskAnalysisConfigForTenantKindNotFound(tenantId: UUID)
      extends ComponentError("0019", s"Risk Analysis Configuration for Tenant ${tenantId.toString} not found")

  final case class RiskAnalysisConfigLatestVersionNotFound(tenantKind: PersistentTenantKind)
      extends ComponentError(
        "0020",
        s"Latest Risk Analysis Configuration for tenant kind ${tenantKind.toString} not found"
      )

  final case class RiskAnalysisConfigVersionNotFound(version: String, tenantKind: PersistentTenantKind)
      extends ComponentError(
        "0021",
        s"Risk Analysis Configuration version $version for tenant kind ${tenantKind.toString} not found"
      )

  final case class DuplicatedPurposeName(name: String)
      extends ComponentError("0022", s"Purpose with name: $name already in use")

  object MissingFreeOfChargeReason extends ComponentError("0023", s"Missing Free Of Charge Reason")

  final case class EServiceNotFound(eServiceId: UUID)
      extends ComponentError("0024", s"EService ${eServiceId.toString} not found")

  final case class EServiceNotInDeliverMode(eServiceId: UUID)
      extends ComponentError("0025", s"EService ${eServiceId.toString} has not Deliver mode")

  final case class EServiceNotInReceiveMode(eServiceId: UUID)
      extends ComponentError("0026", s"EService ${eServiceId.toString} has not Receive mode")
  final case class RiskAnalysisNotFound(eServiceId: UUID, riskAnalysisId: UUID)
      extends ComponentError("0027", s"EService $eServiceId does not contain Risk Analysis $riskAnalysisId")

}
