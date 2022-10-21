package it.pagopa.interop.purposeprocess.api.converters.partymanagement

import it.pagopa.interop.selfcare.partymanagement.client.model.{Institution => DependencyInstitution}
import it.pagopa.interop.purposeprocess.model.Organization

import java.util.UUID

object OrganizationConverter {
  def dependencyToApi(id: UUID, organization: DependencyInstitution): Organization =
    Organization(id = id, name = organization.description)

}
