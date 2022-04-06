package it.pagopa.interop.purposeprocess.api.converters.partymanagement

import it.pagopa.interop.partymanagement.client.model.{Institution => DependencyInstitution}
import it.pagopa.interop.purposeprocess.model.Organization

object OrganizationConverter {
  def dependencyToApi(organization: DependencyInstitution): Organization =
    Organization(id = organization.id, name = organization.description)

}
