package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.RiskAnalysisFormConfig
import it.pagopa.interop.tenantmanagement.model.tenant.{PersistentTenantKind}

import scala.io.Source
import spray.json._

trait RiskAnalysisService {
  def riskAnalysisForms(): Map[PersistentTenantKind, Map[String, RiskAnalysisFormConfig]]
  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig
}

object RiskAnalysisService extends RiskAnalysisService {

  private val riskAnalysisTemplatePath: String = "riskAnalysisTemplate/forms"

  private val riskAnalysisFormsMap: Map[PersistentTenantKind, Map[String, RiskAnalysisFormConfig]] = Map(
    PersistentTenantKind.PA      -> Map(
      "1.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${PersistentTenantKind.PA.toString}/1.0.json"),
      "2.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${PersistentTenantKind.PA.toString}/2.0.json"),
      "3.0" -> loadRiskAnalysisFormConfig(s"$riskAnalysisTemplatePath/${PersistentTenantKind.PA.toString}/3.0.json")
    ),
    PersistentTenantKind.PRIVATE -> Map(
      "1.0" -> loadRiskAnalysisFormConfig(
        s"$riskAnalysisTemplatePath/${PersistentTenantKind.PRIVATE.toString}/1.0.json"
      ),
      "2.0" -> loadRiskAnalysisFormConfig(
        s"$riskAnalysisTemplatePath/${PersistentTenantKind.PRIVATE.toString}/2.0.json"
      )
    ),
    PersistentTenantKind.GSP     -> Map(
      "1.0" -> loadRiskAnalysisFormConfig(
        s"$riskAnalysisTemplatePath/${PersistentTenantKind.PRIVATE.toString}/1.0.json"
      ),
      "2.0" -> loadRiskAnalysisFormConfig(
        s"$riskAnalysisTemplatePath/${PersistentTenantKind.PRIVATE.toString}/2.0.json"
      )
    )
  )

  def riskAnalysisForms(): Map[PersistentTenantKind, Map[String, RiskAnalysisFormConfig]] =
    riskAnalysisFormsMap

  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]

}
