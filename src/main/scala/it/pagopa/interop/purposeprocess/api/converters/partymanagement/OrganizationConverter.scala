package it.pagopa.interop.purposeprocess.api.converters.partymanagement

import it.pagopa.interop.partymanagement.client.model.{Organization => DependencyOrganization}
import it.pagopa.interop.purposeprocess.model.Organization

object OrganizationConverter {
  def dependencyToApi(organization: DependencyOrganization): Organization =
    Organization(id = organization.id, name = organization.description)

}
