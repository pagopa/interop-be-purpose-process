package it.pagopa.pdnd.interop.uservice.purposeprocess.error

sealed trait RiskAnalysisValidationError

final case class UnexpectedField(fieldName: String)       extends RiskAnalysisValidationError
final case class DependencyNotFound(fieldName: String)    extends RiskAnalysisValidationError
final case class TooManyOccurrences(fieldName: String)    extends RiskAnalysisValidationError
final case class MissingExpectedField(fieldName: String)  extends RiskAnalysisValidationError
final case class UnexpectedFieldValue(fieldName: String)  extends RiskAnalysisValidationError
final case class UnexpectedFieldFormat(fieldName: String) extends RiskAnalysisValidationError
