package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{PurposeSeed => DependencyPurposeSeed}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.InternalErrors.RiskAnalysisValidationFailed
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposeSeed

object PurposeSeedConverter {
  def apiToDependency(seed: PurposeSeed): Either[RiskAnalysisValidationFailed, DependencyPurposeSeed] = {
    for {
      riskAnalysisForm <- RiskAnalysisValidation
        .validate(seed.riskAnalysisForm)
        .leftMap(RiskAnalysisValidationFailed)
        .toEither
    } yield DependencyPurposeSeed(
      eserviceId = seed.eserviceId,
      consumerId = seed.consumerId,
      title = seed.title,
      description = seed.description,
      riskAnalysisForm = riskAnalysisForm
    )
  }
}
