package it.pagopa.interop.purposeprocess.api.converters.partymanagement

import it.pagopa.interop.tenantmanagement.client.model.Tenant
import it.pagopa.interop.purposeprocess.model.Organization

import java.util.UUID

object OrganizationConverter {
  def dependencyToApi(id: UUID, tenant: Tenant): Organization =
    Organization(id = id, name = tenant.externalId.origin)

}
