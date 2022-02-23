package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.agreementmanagement

import it.pagopa.pdnd.interop.uservice.agreementmanagement.client.model.{AgreementState => DependencyAgreementState}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.AgreementState

object AgreementStateConverter {
  def dependencyToApi(state: DependencyAgreementState): AgreementState =
    state match {
      case DependencyAgreementState.ACTIVE    => AgreementState.ACTIVE
      case DependencyAgreementState.PENDING   => AgreementState.PENDING
      case DependencyAgreementState.SUSPENDED => AgreementState.SUSPENDED
      case DependencyAgreementState.INACTIVE  => AgreementState.INACTIVE
    }
}
