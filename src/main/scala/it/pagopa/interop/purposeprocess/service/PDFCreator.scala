package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposemanagement.client.model.RiskAnalysisForm
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.{EServiceInfo, Language}
import it.pagopa.interop.tenantmanagement.client.model.TenantKind

import java.io.File
import scala.concurrent.Future

trait PDFCreator {
  def createDocument(
    template: String,
    riskAnalysisForm: RiskAnalysisForm,
    dailyCalls: Int,
    eServiceInfo: EServiceInfo,
    language: Language,
    kind: TenantKind
  ): Future[File]
}
