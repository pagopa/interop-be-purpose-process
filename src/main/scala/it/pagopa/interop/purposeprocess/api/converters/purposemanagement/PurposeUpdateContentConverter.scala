package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.{PurposeUpdateContent => DependencyPurposeUpdateContent}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.RiskAnalysisValidationFailed
import it.pagopa.interop.purposeprocess.model.{RiskAnalysisForm, PurposeUpdateContent}
import it.pagopa.interop.purposemanagement.client.model.RiskAnalysisFormSeed

object PurposeUpdateContentConverter {
  def apiToDependency(
    content: PurposeUpdateContent,
    f: RiskAnalysisForm => RiskAnalysisValidation.ValidationResult[RiskAnalysisFormSeed]
  ): Either[Throwable, DependencyPurposeUpdateContent] = {
    for {
      riskAnalysisForm <- content.riskAnalysisForm
        .traverse(f)
        .leftMap(RiskAnalysisValidationFailed(_))
        .toEither
    } yield DependencyPurposeUpdateContent(
      title = content.title,
      description = content.description,
      riskAnalysisForm = riskAnalysisForm
    )
  }

}
