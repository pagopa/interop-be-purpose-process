package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposemanagement.client.model.RiskAnalysisForm
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.Language

import java.io.File
import scala.concurrent.Future

trait PDFCreator {
  def createDocument(
    template: String,
    riskAnalysisForm: RiskAnalysisForm,
    dailyCalls: Int,
    language: Language
  ): Future[File]
}
