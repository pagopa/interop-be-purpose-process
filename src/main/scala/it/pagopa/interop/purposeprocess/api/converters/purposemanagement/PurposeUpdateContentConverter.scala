package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.{PurposeUpdateContent => DependencyPurposeUpdateContent}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.RiskAnalysisValidationFailed
import it.pagopa.interop.purposeprocess.model.PurposeUpdateContent
import it.pagopa.interop.tenantmanagement.client.model.TenantKind

object PurposeUpdateContentConverter {
  def apiToDependency(content: PurposeUpdateContent, schemaOnlyValidation: Boolean)(
    kind: TenantKind
  ): Either[Throwable, DependencyPurposeUpdateContent] = {
    for {
      riskAnalysisForm <- content.riskAnalysisForm
        .traverse(
          RiskAnalysisValidation
            .validate(_, schemaOnlyValidation = schemaOnlyValidation)(kind)
            .leftMap(RiskAnalysisValidationFailed(_))
            .toEither
        )
    } yield DependencyPurposeUpdateContent(
      title = content.title,
      description = content.description,
      riskAnalysisForm = riskAnalysisForm
    )
  }

}
