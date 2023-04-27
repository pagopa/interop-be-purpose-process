package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.{PurposeSeed => DependencyPurposeSeed}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.RiskAnalysisValidationFailed
import it.pagopa.interop.purposeprocess.model.PurposeSeed
import it.pagopa.interop.tenantmanagement.client.model.TenantKind

object PurposeSeedConverter {

  def apiToDependency(
    seed: PurposeSeed,
    schemaOnlyValidation: Boolean
  )(kind: TenantKind): Either[RiskAnalysisValidationFailed, DependencyPurposeSeed] =
    for {
      riskAnalysisFormSeed <- seed.riskAnalysisForm
        .traverse(
          RiskAnalysisValidation
            .validate(_, schemaOnlyValidation = schemaOnlyValidation)(kind)
            .leftMap(RiskAnalysisValidationFailed(_))
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
