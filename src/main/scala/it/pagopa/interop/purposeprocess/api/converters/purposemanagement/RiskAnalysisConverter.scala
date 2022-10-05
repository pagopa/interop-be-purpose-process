package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysis,
  RiskAnalysisMultiAnswer => DepRiskAnalysisMultiAnswer,
  RiskAnalysisSingleAnswer => DepRiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.model.RiskAnalysisForm

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

}
