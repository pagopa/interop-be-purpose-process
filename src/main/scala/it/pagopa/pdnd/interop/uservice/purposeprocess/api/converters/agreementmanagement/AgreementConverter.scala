package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.agreementmanagement

import it.pagopa.pdnd.interop.uservice.agreementmanagement.client.model.{Agreement => DependencyAgreement}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.Agreement

object AgreementConverter {
  def dependencyToApi(agreement: DependencyAgreement): Agreement =
    Agreement(id = agreement.id, state = AgreementStateConverter.dependencyToApi(agreement.state))
}
