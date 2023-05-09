package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.RiskAnalysisFormConfig
import it.pagopa.interop.tenantmanagement.client.model.TenantKind
import it.pagopa.interop.tenantmanagement.client.model.TenantKind.{GSP, PA, PRIVATE}

import scala.io.Source
import spray.json._

trait RiskAnalysisService {
  def riskAnalysisForms(): Map[TenantKind, Map[String, RiskAnalysisFormConfig]]
  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig
}

object RiskAnalysisService extends RiskAnalysisService {

  private val riskAnalysisTemplatePath: String = "riskAnalysisTemplate/forms"

  private val riskAnalysisFormsMap: Map[TenantKind, Map[String, RiskAnalysisFormConfig]] = Map(
    PA      -> Map(
      "1.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.PA.toString}/1.0.json"),
      "2.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.PA.toString}/2.0.json")
    ),
    PRIVATE -> Map(
      "2.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.PRIVATE.toString}/2.0.json")
    ),
    GSP     -> Map(
      "2.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.PRIVATE.toString}/2.0.json")
    )
  )

  def riskAnalysisForms(): Map[TenantKind, Map[String, RiskAnalysisFormConfig]] =
    riskAnalysisFormsMap

  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]

}
