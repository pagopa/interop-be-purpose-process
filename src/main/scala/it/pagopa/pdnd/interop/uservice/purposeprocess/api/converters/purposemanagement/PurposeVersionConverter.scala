package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{PurposeVersion => DependencyPurposeVersion}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposeVersion

object PurposeVersionConverter {
  def dependencyToApi(version: DependencyPurposeVersion): PurposeVersion =
    PurposeVersion(
      id = version.id,
      state = PurposeVersionStateConverter.dependencyToApi(version.state),
      createdAt = version.createdAt,
      updatedAt = version.updatedAt,
      expectedApprovalDate = version.expectedApprovalDate,
      riskAnalysis = version.riskAnalysis.map(PurposeVersionDocumentConverter.dependencyToApi)
    )
}
