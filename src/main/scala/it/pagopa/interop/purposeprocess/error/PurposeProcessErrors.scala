package it.pagopa.interop.purposeprocess.error

import cats.data.NonEmptyChain
import it.pagopa.interop.commons.utils.errors.ComponentError

import java.util.UUID

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
    def apply(failures: NonEmptyChain[RiskAnalysisValidationError]): RiskAnalysisValidationFailed =
      RiskAnalysisValidationFailed(failures.map(_.message).distinct.iterator.mkString("[", ", ", "]"))
  }

  final case class AgreementNotFound(eServiceId: String, consumerId: String)
      extends ComponentError("0005", s"No Agreement found for EService $eServiceId and Consumer $consumerId")
  final case class DescriptorNotFound(eServiceId: String, descriptorId: String)
      extends ComponentError("0006", s"Descriptor $descriptorId not found for EService $eServiceId")

  final case class MissingRiskAnalysis(purposeId: UUID, versionId: UUID)
      extends ComponentError("0007", s"Version $versionId of Purpose $purposeId must contain a valid risk analysis")

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

  final case class TenantNotFound(tenantId: UUID)
      extends ComponentError("0015", s"Tenant ${tenantId.toString} not found")
  
  final case class TenantKindNotFound(tenantId: UUID)
      extends ComponentError("0016", s"Tenant kind for tenant ${tenantId.toString} not found")    
}
