package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{PurposeVersion => DependencyPurposeVersion}
import it.pagopa.interop.purposemanagement.model.purpose.PersistentPurposeVersion
import it.pagopa.interop.purposeprocess.model.PurposeVersion

object PurposeVersionConverter {
  def dependencyToApi(version: DependencyPurposeVersion): PurposeVersion =
    PurposeVersion(
      id = version.id,
      state = PurposeVersionStateConverter.dependencyToApi(version.state),
      createdAt = version.createdAt,
      updatedAt = version.updatedAt,
      firstActivationAt = version.firstActivationAt,
      expectedApprovalDate = version.expectedApprovalDate,
      riskAnalysis = version.riskAnalysis.map(PurposeVersionDocumentConverter.dependencyToApi),
      dailyCalls = version.dailyCalls
    )

  def persistentToApi(version: PersistentPurposeVersion): PurposeVersion =
    PurposeVersion(
      id = version.id,
      state = PurposeVersionStateConverter.persistentToApi(version.state),
      createdAt = version.createdAt,
      updatedAt = version.updatedAt,
      firstActivationAt = version.firstActivationAt,
      expectedApprovalDate = version.expectedApprovalDate,
      riskAnalysis = version.riskAnalysis.map(PurposeVersionDocumentConverter.persistentToApi),
      dailyCalls = version.dailyCalls
    )
}
