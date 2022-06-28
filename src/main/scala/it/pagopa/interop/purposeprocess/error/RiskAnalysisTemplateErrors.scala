package it.pagopa.interop.purposeprocess.error

object RiskAnalysisTemplateErrors {
  final case class FormTemplateConfigNotFound(templateVersion: String)
      extends Throwable(s"Config version $templateVersion not found")

  final case class IncompatibleConfig(questionId: String, configId: String)
      extends Throwable(s"Question $questionId not compatible with config $configId")

  final case class UnexpectedEmptyAnswer(questionId: String)
      extends Throwable(s"Unexpected empty answer for $questionId")

  final case class AnswerNotFoundInConfig(questionId: String, configId: String)
      extends Throwable(s"Answer $questionId not found in config $configId")

  final case class QuestionNotFoundInConfig(questionId: String, configVersion: String)
      extends Throwable(s"Question $questionId not found in configuration with version $configVersion")

}
