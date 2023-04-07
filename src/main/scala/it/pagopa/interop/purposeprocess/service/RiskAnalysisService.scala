package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.RiskAnalysisFormConfig
import it.pagopa.interop.tenantmanagement.client.model.TenantKind
import it.pagopa.interop.tenantmanagement.client.model.TenantKind.{GSP, PA, PRIVATE}

import scala.io.Source
import spray.json._

object RiskAnalysisService {

  // The Map key must correspond to the version field of the risk analysis form
  val riskAnalysisForms: Map[TenantKind, Map[String, RiskAnalysisFormConfig]] = Map(
    PA      -> Map(
      "1.0" -> loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/1.0.json"),
      "2.0" -> loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/2.0.json")
    ),
    PRIVATE -> Map("1.0" -> loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/1.0.json")),
    GSP     -> Map("1.0" -> loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/1.0.json"))
  )

  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]

}
