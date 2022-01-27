package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Purposes => DependencyPurposes}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.Purposes

object PurposesConverter {
  def dependencyToApi(purposes: DependencyPurposes): Purposes =
    Purposes(purposes = purposes.purposes.map(PurposeConverter.dependencyToApi))
}
