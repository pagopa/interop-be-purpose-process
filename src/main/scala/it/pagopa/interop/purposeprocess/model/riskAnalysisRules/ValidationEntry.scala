package it.pagopa.interop.purposeprocess.model.riskAnalysisRules

import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.DataType

final case class ValidationEntry(
  fieldName: String,
  dataType: DataType,
  required: Boolean,
  dependencies: Seq[DependencyEntry],
  allowedValues: Option[Set[String]]
)
