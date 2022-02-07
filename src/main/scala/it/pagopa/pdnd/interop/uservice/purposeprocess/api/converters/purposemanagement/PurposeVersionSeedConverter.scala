package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  PurposeVersionSeed => DependencyPurposeVersionSeed
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposeVersionSeed

object PurposeVersionSeedConverter {
  def apiToDependency(seed: PurposeVersionSeed): DependencyPurposeVersionSeed =
    DependencyPurposeVersionSeed(seed.dailyCalls, None)
}
