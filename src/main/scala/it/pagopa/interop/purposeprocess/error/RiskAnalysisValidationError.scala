package it.pagopa.interop.purposeprocess.error

import cats.Show

sealed trait RiskAnalysisValidationError {
  def message: String
}

object RiskAnalysisValidationError {
  implicit val showPerson: Show[RiskAnalysisValidationError] = Show.show(_.message)

}

final case class UnexpectedField(fieldName: String)                            extends RiskAnalysisValidationError {
  val message: String = s"Unexpected field $fieldName"
}
final case class DependencyNotFound(fieldName: String, dependentField: String) extends RiskAnalysisValidationError {
  val message: String = s"Field $dependentField expects field $fieldName to be in the form"
}
final case class TooManyOccurrences(fieldName: String)                         extends RiskAnalysisValidationError {
  val message: String = s"Too many occurrences of field $fieldName"
}
final case class MissingExpectedField(fieldName: String)                       extends RiskAnalysisValidationError {
  val message: String = s"Expected field $fieldName not found in form"
}
final case class UnexpectedFieldValueByDependency(fieldName: String, dependentField: String, expectedValue: String)
    extends RiskAnalysisValidationError {
  val message: String = s"Field $dependentField requires field $fieldName value to be $expectedValue"
}
final case class UnexpectedFieldValue(fieldName: String, allowedValues: Option[Set[String]])
    extends RiskAnalysisValidationError {
  val message: String = s"Field $fieldName should be one of ${allowedValues.fold("empty")(_.mkString("[", ",", "]"))}"
}
final case class UnexpectedFieldFormat(fieldName: String)                      extends RiskAnalysisValidationError {
  val message: String = s"Unexpected format for field $fieldName"
}
final case class UnexpectedTemplateVersion(templateVersion: String)            extends RiskAnalysisValidationError {
  val message: String = s"Unexpected template version $templateVersion"
}
