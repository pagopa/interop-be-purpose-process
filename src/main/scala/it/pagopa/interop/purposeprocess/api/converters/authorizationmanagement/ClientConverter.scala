package it.pagopa.interop.purposeprocess.api.converters.authorizationmanagement

import it.pagopa.interop.authorizationmanagement.client.model.{Client => DependencyClient}
import it.pagopa.interop.purposeprocess.model.Client

object ClientConverter {
  def dependencyToApi(client: DependencyClient): Client = Client(id = client.id, name = client.name)
}
