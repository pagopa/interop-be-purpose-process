package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.RiskAnalysisForm

import java.io.File
import scala.concurrent.Future

trait PDFCreator {
  def createDocument(template: String, riskAnalysisForm: RiskAnalysisForm, dailyCalls: Int): Future[File]
}
