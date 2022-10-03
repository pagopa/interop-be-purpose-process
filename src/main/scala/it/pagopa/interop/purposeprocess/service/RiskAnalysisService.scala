package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.RiskAnalysisFormConfig

import scala.io.Source
import spray.json._

object RiskAnalysisService {

  // The Map key must correspond to the version field of the risk analysis form
  val riskAnalysisForms: Map[String, RiskAnalysisFormConfig] = Map(
    "1.0" -> loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/1.0.json"),
    "2.0" -> loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/2.0.json")
  )

  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]

}
