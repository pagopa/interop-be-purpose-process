package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposemanagement.model.purpose.PersistentRiskAnalysisForm
import it.pagopa.interop.purposeprocess.model.EServiceInfo
import it.pagopa.interop.commons.riskanalysis.model.riskAnalysisTemplate.Language
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind

import java.io.File
import scala.concurrent.Future

trait PDFCreator {
  def createDocument(
    template: String,
    riskAnalysisForm: PersistentRiskAnalysisForm,
    dailyCalls: Int,
    eServiceInfo: EServiceInfo,
    isFreeOfCharge: Boolean,
    freeOfChargeReason: Option[String],
    language: Language
  )(kind: PersistentTenantKind): Future[File]
}
