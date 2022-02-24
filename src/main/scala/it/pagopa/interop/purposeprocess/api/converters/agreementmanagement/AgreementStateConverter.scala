package it.pagopa.interop.purposeprocess.api.converters.agreementmanagement

import it.pagopa.interop.agreementmanagement.client.model.{AgreementState => DependencyAgreementState}
import it.pagopa.interop.purposeprocess.model.AgreementState

object AgreementStateConverter {
  def dependencyToApi(state: DependencyAgreementState): AgreementState =
    state match {
      case DependencyAgreementState.ACTIVE    => AgreementState.ACTIVE
      case DependencyAgreementState.PENDING   => AgreementState.PENDING
      case DependencyAgreementState.SUSPENDED => AgreementState.SUSPENDED
      case DependencyAgreementState.INACTIVE  => AgreementState.INACTIVE
    }
}
