package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{
  WaitingForApprovalPurposeVersionUpdateContent => DependencyWaitingForApprovalPurposeVersionUpdateContent
}
import it.pagopa.interop.purposeprocess.model.WaitingForApprovalPurposeVersionUpdateContent

object WaitingForApprovalPurposeVersionUpdateContentConverter {
  def dependencyToApi(
    updateContent: DependencyWaitingForApprovalPurposeVersionUpdateContent
  ): WaitingForApprovalPurposeVersionUpdateContent = WaitingForApprovalPurposeVersionUpdateContent(
    updateContent.expectedApprovalDate
  )

  def apiToDependency(
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  ): DependencyWaitingForApprovalPurposeVersionUpdateContent = DependencyWaitingForApprovalPurposeVersionUpdateContent(
    updateContent.expectedApprovalDate
  )
}
