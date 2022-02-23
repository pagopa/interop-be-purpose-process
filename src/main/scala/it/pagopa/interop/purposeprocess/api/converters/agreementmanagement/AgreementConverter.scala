package it.pagopa.interop.purposeprocess.api.converters.agreementmanagement

import it.pagopa.interop.agreementmanagement.client.model.{Agreement => DependencyAgreement}
import it.pagopa.interop.purposeprocess.model.Agreement

object AgreementConverter {
  def dependencyToApi(agreement: DependencyAgreement): Agreement =
    Agreement(id = agreement.id, state = AgreementStateConverter.dependencyToApi(agreement.state))
}
