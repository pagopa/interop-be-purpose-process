package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  DraftPurposeVersionUpdateContent => DependencyDraftPurposeVersionUpdateContent
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.DraftPurposeVersionUpdateContent

object DraftPurposeVersionUpdateContentConverter {
  def dependencyToApi(updateContent: DependencyDraftPurposeVersionUpdateContent): DraftPurposeVersionUpdateContent =
    DraftPurposeVersionUpdateContent(dailyCalls = updateContent.dailyCalls)

  def apiToDependency(updateContent: DraftPurposeVersionUpdateContent): DependencyDraftPurposeVersionUpdateContent =
    DependencyDraftPurposeVersionUpdateContent(dailyCalls = updateContent.dailyCalls)
}
