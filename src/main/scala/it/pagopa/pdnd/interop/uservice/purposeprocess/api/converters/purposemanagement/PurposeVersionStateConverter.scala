package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  PurposeVersionState => DependencyPurposeVersionState
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposeVersionState

object PurposeVersionStateConverter {
  def dependencyToApi(state: DependencyPurposeVersionState): PurposeVersionState =
    state match {
      case DependencyPurposeVersionState.ACTIVE               => PurposeVersionState.ACTIVE
      case DependencyPurposeVersionState.DRAFT                => PurposeVersionState.DRAFT
      case DependencyPurposeVersionState.SUSPENDED            => PurposeVersionState.SUSPENDED
      case DependencyPurposeVersionState.WAITING_FOR_APPROVAL => PurposeVersionState.WAITING_FOR_APPROVAL
      case DependencyPurposeVersionState.ARCHIVED             => PurposeVersionState.ARCHIVED
    }
}
