package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  WaitingForApprovalPurposeVersionUpdateContent => DependencyWaitingForApprovalPurposeVersionUpdateContent
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.WaitingForApprovalPurposeVersionUpdateContent

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
