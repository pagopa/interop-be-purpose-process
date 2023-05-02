package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.RiskAnalysisFormConfig
import it.pagopa.interop.tenantmanagement.client.model.TenantKind

trait RiskAnalysisService {
  def riskAnalysisForms(): Map[TenantKind, Map[String, RiskAnalysisFormConfig]]
  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig
}
