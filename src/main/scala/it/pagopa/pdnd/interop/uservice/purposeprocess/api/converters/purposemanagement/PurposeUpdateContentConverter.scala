package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  PurposeUpdateContent => DependencyPurposeUpdateContent
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.pdnd.interop.uservice.purposeprocess.error.InternalErrors.RiskAnalysisValidationFailed
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposeUpdateContent
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
