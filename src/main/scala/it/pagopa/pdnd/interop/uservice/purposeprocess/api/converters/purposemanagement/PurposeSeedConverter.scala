package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{PurposeSeed => DependencyPurposeSeed}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposeSeed

object PurposeSeedConverter {
  def apiToDependency(seed: PurposeSeed): DependencyPurposeSeed =
    DependencyPurposeSeed(
      eserviceId = seed.eserviceId,
      consumerId = seed.consumerId,
      title = seed.title,
      description = seed.description
    )
}
