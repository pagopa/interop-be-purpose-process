package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{PurposeVersionState => DependencyPurposeVersionState}
import it.pagopa.interop.purposemanagement.model.purpose._
import it.pagopa.interop.purposeprocess.model.PurposeVersionState

object PurposeVersionStateConverter {
  def dependencyToApi(state: DependencyPurposeVersionState): PurposeVersionState =
    state match {
      case DependencyPurposeVersionState.ACTIVE               => PurposeVersionState.ACTIVE
      case DependencyPurposeVersionState.DRAFT                => PurposeVersionState.DRAFT
      case DependencyPurposeVersionState.SUSPENDED            => PurposeVersionState.SUSPENDED
      case DependencyPurposeVersionState.WAITING_FOR_APPROVAL => PurposeVersionState.WAITING_FOR_APPROVAL
      case DependencyPurposeVersionState.ARCHIVED             => PurposeVersionState.ARCHIVED
    }

  def persistentToApi(state: PersistentPurposeVersionState): PurposeVersionState =
    state match {
      case Active             => PurposeVersionState.ACTIVE
      case Draft              => PurposeVersionState.DRAFT
      case Suspended          => PurposeVersionState.SUSPENDED
      case WaitingForApproval => PurposeVersionState.WAITING_FOR_APPROVAL
      case Archived           => PurposeVersionState.ARCHIVED
    }

  def apiToPersistent(state: PurposeVersionState): PersistentPurposeVersionState =
    state match {
      case PurposeVersionState.ACTIVE               => Active
      case PurposeVersionState.DRAFT                => Draft
      case PurposeVersionState.SUSPENDED            => Suspended
      case PurposeVersionState.WAITING_FOR_APPROVAL => WaitingForApproval
      case PurposeVersionState.ARCHIVED             => Archived
    }
}
