package it.pagopa.interop.purposeprocess.error

import cats.Show

sealed trait RiskAnalysisValidationError {
  def message: String
}

object RiskAnalysisValidationError {
  implicit val showPerson: Show[RiskAnalysisValidationError] = Show.show(_.message)

}

final case class UnexpectedField(fieldName: String) extends RiskAnalysisValidationError {
  val message: String = s"Unexpected field $fieldName"
}
final case class DependencyNotFound(fieldName: String) extends RiskAnalysisValidationError {
  val message: String = s"Dependency $fieldName not found"
}
final case class TooManyOccurrences(fieldName: String) extends RiskAnalysisValidationError {
  val message: String = s"Too many occurrences of field $fieldName"
}
final case class MissingExpectedField(fieldName: String) extends RiskAnalysisValidationError {
  val message: String = s"Expected field $fieldName not found in form"
}
final case class UnexpectedFieldValue(fieldName: String) extends RiskAnalysisValidationError {
  val message: String = s"Unexpected value for field $fieldName"
}
final case class UnexpectedFieldFormat(fieldName: String) extends RiskAnalysisValidationError {
  val message: String = s"Unexpected format for field $fieldName"
}
