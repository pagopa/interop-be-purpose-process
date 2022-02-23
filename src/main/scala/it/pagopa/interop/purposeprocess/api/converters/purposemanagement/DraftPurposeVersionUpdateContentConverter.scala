package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{
  DraftPurposeVersionUpdateContent => DependencyDraftPurposeVersionUpdateContent
}
import it.pagopa.interop.purposeprocess.model.DraftPurposeVersionUpdateContent

object DraftPurposeVersionUpdateContentConverter {
  def dependencyToApi(updateContent: DependencyDraftPurposeVersionUpdateContent): DraftPurposeVersionUpdateContent =
    DraftPurposeVersionUpdateContent(dailyCalls = updateContent.dailyCalls)

  def apiToDependency(updateContent: DraftPurposeVersionUpdateContent): DependencyDraftPurposeVersionUpdateContent =
    DependencyDraftPurposeVersionUpdateContent(dailyCalls = updateContent.dailyCalls)
}
