package it.pagopa.interop.purposeprocess.server.impl

import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisForm,
  RiskAnalysisMultiAnswer,
  RiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate._
import it.pagopa.interop.purposeprocess.service.impl.PDFCreatorImpl

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.{Failure, Success}

object TestDocument extends App {
  val pdfCreator = PDFCreatorImpl

  private[this] val riskAnalysisTemplate = Source
    .fromResource("riskAnalysisTemplate/index.html")
    .getLines()
    .mkString(System.lineSeparator())

  val riskAnalysisForm: RiskAnalysisForm =
    RiskAnalysisForm(
      UUID.randomUUID(),
      "1.0",
      singleAnswers = Seq(RiskAnalysisSingleAnswer(UUID.randomUUID(), "purpose", Some("THIS. IS. PURPOOOOSE"))),
      multiAnswers = Seq(RiskAnalysisMultiAnswer(UUID.randomUUID(), "legalBasis", Seq("CONSENT", "LEGAL_OBLIGATION")))
    )
  val dailyCalls                         = 1000

  pdfCreator.createDocument(riskAnalysisTemplate, riskAnalysisForm, dailyCalls, LanguageIt).onComplete {
    case Success(value) => println(value.getPath)
    case Failure(err)   => println(err)
  }
}
