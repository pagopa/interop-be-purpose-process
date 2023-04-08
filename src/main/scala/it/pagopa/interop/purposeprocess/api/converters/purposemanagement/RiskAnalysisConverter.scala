package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysis,
  RiskAnalysisMultiAnswer => DepRiskAnalysisMultiAnswer,
  RiskAnalysisSingleAnswer => DepRiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposemanagement.model.purpose.{
  PersistentRiskAnalysisForm,
  PersistentRiskAnalysisMultiAnswer,
  PersistentRiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.model.{
  RiskAnalysisForm,
  RiskAnalysisFormConfigResponse,
  FormConfigQuestionResponse,
  LocalizedTextResponse,
  DataTypeResponse,
  DependencyResponse
}
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.{
  RiskAnalysisFormConfig,
  FormConfigQuestion,
  LocalizedText,
  DataType,
  Single,
  Multi,
  FreeText,
  Dependency
}

object RiskAnalysisConverter {
  def dependencyToApi(riskAnalysis: DepRiskAnalysis): RiskAnalysisForm =
    RiskAnalysisForm(
      version = riskAnalysis.version,
      answers = singleAnswersToApi(riskAnalysis.singleAnswers) ++ multiAnswersToApi(riskAnalysis.multiAnswers)
    )

  def singleAnswersToApi(singleAnswers: Seq[DepRiskAnalysisSingleAnswer]): Map[String, Seq[String]] =
    singleAnswers.map(a => (a.key, a.value.toSeq)).toMap

  def multiAnswersToApi(multiAnswers: Seq[DepRiskAnalysisMultiAnswer]): Map[String, Seq[String]] =
    multiAnswers.map(a => (a.key, a.values)).toMap

  def persistentToApi(riskAnalysis: PersistentRiskAnalysisForm): RiskAnalysisForm =
    RiskAnalysisForm(
      version = riskAnalysis.version,
      answers = persistentSingleAnswersToApi(riskAnalysis.singleAnswers) ++ persistentMultiAnswersToApi(
        riskAnalysis.multiAnswers
      )
    )

  def persistentSingleAnswersToApi(singleAnswers: Seq[PersistentRiskAnalysisSingleAnswer]): Map[String, Seq[String]] =
    singleAnswers.map(a => (a.key, a.value.toSeq)).toMap

  def persistentMultiAnswersToApi(multiAnswers: Seq[PersistentRiskAnalysisMultiAnswer]): Map[String, Seq[String]] =
    multiAnswers.map(a => (a.key, a.values)).toMap

  def toResponse(riskAnalysisFormConfig: RiskAnalysisFormConfig): RiskAnalysisFormConfigResponse =
    RiskAnalysisFormConfigResponse(
      version = riskAnalysisFormConfig.version,
      questions = riskAnalysisFormConfig.questions.map(toResponse)
    )

  def toResponse(question: FormConfigQuestion): FormConfigQuestionResponse =
    FormConfigQuestionResponse(
      id = question.id,
      label = toResponse(question.label),
      infoLabel = question.infoLabel.map(toResponse),
      dataType = toResponse(question.dataType),
      required = question.required,
      dependencies = question.dependencies.map(toResponse)
    )

  def toResponse(localizedText: LocalizedText): LocalizedTextResponse =
    LocalizedTextResponse(it = localizedText.it, en = localizedText.en)

  def toResponse(dataType: DataType): DataTypeResponse =
    dataType match {
      case Single   => DataTypeResponse.SINGLE
      case Multi    => DataTypeResponse.MULTI
      case FreeText => DataTypeResponse.FREETEXT
    }

  def toResponse(dependency: Dependency): DependencyResponse =
    DependencyResponse(id = dependency.id, value = dependency.value)
}
