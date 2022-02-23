package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.{PurposeSeed => DependencyPurposeSeed}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.error.InternalErrors.RiskAnalysisValidationFailed
import it.pagopa.interop.purposeprocess.model.PurposeSeed

object PurposeSeedConverter {

  def apiToDependency(seed: PurposeSeed): Either[RiskAnalysisValidationFailed, DependencyPurposeSeed] =
    for {
      riskAnalysisFormSeed <- seed.riskAnalysisForm
        .traverse(
          RiskAnalysisValidation
            .validate(_)
            .leftMap(RiskAnalysisValidationFailed)
            .toEither
        )
    } yield DependencyPurposeSeed(
      eserviceId = seed.eserviceId,
      consumerId = seed.consumerId,
      title = seed.title,
      description = seed.description,
      riskAnalysisForm = riskAnalysisFormSeed
    )
}
