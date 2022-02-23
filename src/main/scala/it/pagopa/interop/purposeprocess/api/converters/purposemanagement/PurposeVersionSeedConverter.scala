package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{PurposeVersionSeed => DependencyPurposeVersionSeed}
import it.pagopa.interop.purposeprocess.model.PurposeVersionSeed

object PurposeVersionSeedConverter {
  def apiToDependency(seed: PurposeVersionSeed): DependencyPurposeVersionSeed =
    DependencyPurposeVersionSeed(seed.dailyCalls, None)
}
