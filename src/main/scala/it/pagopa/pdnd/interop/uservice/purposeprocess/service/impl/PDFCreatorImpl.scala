package it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl

import com.openhtmltopdf.util.XRLog
import it.pagopa.pdnd.interop.commons.files.service.PDFManager
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.RiskAnalysisForm
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.PDFCreator

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.Future
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.Try

object PDFCreatorImpl extends PDFCreator with PDFManager {

  // Suppressing openhtmltopdf log
  XRLog.listRegisteredLoggers.asScala.foreach((logger: String) =>
    XRLog.setLevel(logger, java.util.logging.Level.SEVERE)
  )

  override def createDocument(template: String, riskAnalysisForm: RiskAnalysisForm, dailyCalls: Int): Future[File] =
    Future.fromTry {
      for {
        file <- createTempFile
        data = setupData(riskAnalysisForm, dailyCalls)
        pdf <- getPDFAsFile(file.toPath, template, data)
      } yield pdf

    }

  private def createTempFile: Try[File] = {
    Try {
      val fileTimestamp: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
      File.createTempFile(s"${fileTimestamp}_${UUID.randomUUID().toString}_risk_analysis.", ".pdf")
    }
  }

  // TODO This implementation depends on the template (to be defined)
  private def setupData(riskAnalysisForm: RiskAnalysisForm, dailyCalls: Int): Map[String, String] = {
    Map("dailyCalls" -> dailyCalls.toString) ++
      riskAnalysisForm.singleAnswers.map(answer => answer.key -> answer.value.getOrElse("")).toMap ++
      riskAnalysisForm.multiAnswers
        .flatMap(answer => answer.values.map(value => s"${answer.key}_$value" -> value))
        .toMap
  }

}
