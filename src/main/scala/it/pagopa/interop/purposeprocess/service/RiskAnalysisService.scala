package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.RiskAnalysisFormConfig
import it.pagopa.interop.tenantmanagement.client.model.TenantKind
import it.pagopa.interop.tenantmanagement.client.model.TenantKind.{GSP, PA, PRIVATE}

import scala.io.Source
import spray.json._

trait RiskAnalysisServiceSupplier {
  def get(): RiskAnalysisService
}

object RiskAnalysisServiceSupplier extends RiskAnalysisServiceSupplier {
  override def get(): RiskAnalysisService = new RiskAnalysisServiceImpl
}

trait RiskAnalysisService {
  def riskAnalysisForms(): Map[TenantKind, Map[String, RiskAnalysisFormConfig]]
  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig
}
class RiskAnalysisServiceImpl extends RiskAnalysisService {
  override def riskAnalysisForms(): Map[TenantKind, Map[String, RiskAnalysisFormConfig]] =
    RiskAnalysisServiceImpl.riskAnalysisForms
  override def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig  =
    RiskAnalysisServiceImpl.loadRiskAnalysisFormConfig(resourcePath)
}

private object RiskAnalysisServiceImpl {

  private val riskAnalysisTemplatePath: String = "riskAnalysisTemplate/forms"

  private def riskAnalysisForms: Map[TenantKind, Map[String, RiskAnalysisFormConfig]] = Map(
    PA      -> Map(
      "1.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.PA.toString}/1.0.json"),
      "2.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.PA.toString}/2.0.json")
    ),
    PRIVATE -> Map(
      "1.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.PRIVATE.toString}/1.0.json")
    ),
    GSP -> Map("1.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${TenantKind.GSP.toString}/1.0.json"))
  )

  private def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]

}
