package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{PurposeUpdateContent => DependencyPurposeUpdateContent}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.error.InternalErrors.RiskAnalysisValidationFailed
import it.pagopa.interop.purposeprocess.model.PurposeUpdateContent
import cats.implicits._

object PurposeUpdateContentConverter {
  def apiToDependency(content: PurposeUpdateContent): Either[Throwable, DependencyPurposeUpdateContent] = {
    for {
      riskAnalysisForm <- content.riskAnalysisForm
        .traverse(RiskAnalysisValidation.validate)
        .leftMap(RiskAnalysisValidationFailed)
        .toEither
    } yield DependencyPurposeUpdateContent(
      title = content.title,
      description = content.description,
      riskAnalysisForm = riskAnalysisForm
    )
  }

}
