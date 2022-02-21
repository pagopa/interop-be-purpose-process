package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.partymanagement

import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.{Organization => DependencyOrganization}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.Organization

object OrganizationConverter {
  def dependencyToApi(organization: DependencyOrganization): Organization =
    Organization(id = organization.id, name = organization.description)

}
