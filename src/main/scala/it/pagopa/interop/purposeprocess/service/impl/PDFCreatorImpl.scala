package it.pagopa.interop.purposeprocess.service.impl

import cats.implicits.toTraverseOps
import com.openhtmltopdf.util.XRLog
import it.pagopa.interop.commons.files.model.PDFConfiguration
import it.pagopa.interop.commons.files.service.PDFManager
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisForm,
  RiskAnalysisMultiAnswer,
  RiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.error.RiskAnalysisTemplateErrors._
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate._
import it.pagopa.interop.purposeprocess.service.{PDFCreator, RiskAnalysisService}

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Try}

object PDFCreatorImpl extends PDFCreator with PDFManager {

  // Suppressing openhtmltopdf log
  XRLog.listRegisteredLoggers.asScala.foreach((logger: String) =>
    XRLog.setLevel(logger, java.util.logging.Level.SEVERE)
  )

  private[this] val printedDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  private[this] val pdfConfigs: PDFConfiguration = PDFConfiguration(resourcesBaseUrl = Some("/riskAnalysisTemplate/"))

  override def createDocument(
    template: String,
    riskAnalysisForm: RiskAnalysisForm,
    dailyCalls: Int,
    eServiceInfo: EServiceInfo,
    language: Language
  ): Future[File] =
    Future.fromTry {
      for {
        file       <- createTempFile
        formConfig <- RiskAnalysisService.riskAnalysisForms
          .get(riskAnalysisForm.version)
          .toTry(FormTemplateConfigNotFound(riskAnalysisForm.version))
        data       <- setupData(formConfig, riskAnalysisForm, dailyCalls, eServiceInfo, language)
        pdf        <- getPDFAsFileWithConfigs(file.toPath, template, data, pdfConfigs)
      } yield pdf
    }

  def setupData(
    formConfig: RiskAnalysisFormConfig,
    riskAnalysisForm: RiskAnalysisForm,
    dailyCalls: Int,
    eServiceInfo: EServiceInfo,
    language: Language
  ): Try[Map[String, String]] =
    for {
      answers <- sortedAnswers(formConfig, riskAnalysisForm, language)
    } yield Map(
      "dailyCalls"   -> dailyCalls.toString,
      "answers"      -> answers.mkString("\n"),
      "eServiceName" -> eServiceInfo.name,
      "producerName" -> eServiceInfo.producerName,
      "consumerName" -> eServiceInfo.consumerName,
      "date"         -> LocalDateTime.now().format(printedDateFormatter)
    )

  def sortedAnswers(
    formConfig: RiskAnalysisFormConfig,
    riskAnalysisForm: RiskAnalysisForm,
    language: Language
  ): Try[List[String]] =
    formConfig.questions
      .flatMap(config =>
        riskAnalysisForm.singleAnswers.find(_.key == config.id).map(formatSingleAnswer(config, language)) orElse
          riskAnalysisForm.multiAnswers.find(_.key == config.id).map(formatMultiAnswer(config, language))
      )
      .sequence

  private[this] def formatSingleAnswer(formConfig: FormConfigQuestion, language: Language)(
    answer: RiskAnalysisSingleAnswer
  ): Try[String] = formatAnswer(formConfig, language, answer, getSingleAnswerText(language))

  private[this] def formatMultiAnswer(formConfig: FormConfigQuestion, language: Language)(
    answer: RiskAnalysisMultiAnswer
  ): Try[String] = formatAnswer(formConfig, language, answer, getMultiAnswerText(language))

  private[this] def formatAnswer[T](
    questionConfig: FormConfigQuestion,
    language: Language,
    answer: T,
    getText: (FormConfigQuestion, T) => Try[String]
  ): Try[String] = {
    val questionLabel = getLocalizedLabel(questionConfig.label, language)
    val infoLabel     = questionConfig.infoLabel.map(getLocalizedLabel(_, language))
    getText(questionConfig, answer).map(answerToHtml(questionLabel, infoLabel, _))
  }

  private[this] def getSingleAnswerText(
    language: Language
  )(questionConfig: FormConfigQuestion, answer: RiskAnalysisSingleAnswer): Try[String] =
    questionConfig match {
      case _: FreeInputQuestion => answer.value.toTry(UnexpectedEmptyAnswer(answer.key))
      case c: SingleQuestion    => getSingleAnswerTextFromConfig(c, answer, language)
      case c: MultiQuestion     => Failure(IncompatibleConfig(answer.key, c.id))
    }

  private[this] def getMultiAnswerText(
    language: Language
  )(questionConfig: FormConfigQuestion, answer: RiskAnalysisMultiAnswer): Try[String] =
    questionConfig match {
      case c: FreeInputQuestion => Failure(IncompatibleConfig(answer.key, c.id))
      case c: SingleQuestion    => Failure(IncompatibleConfig(answer.key, c.id))
      case c: MultiQuestion     => getMultiAnswerTextFromConfig(c, answer, language)
    }

  private[this] def getSingleAnswerTextFromConfig(
    questionConfig: SingleQuestion,
    answer: RiskAnalysisSingleAnswer,
    language: Language
  ): Try[String] = for {
    answerValue  <- answer.value.toTry(UnexpectedEmptyAnswer(answer.key))
    labeledValue <- questionConfig.options
      .find(_.value == answerValue)
      .toTry(AnswerNotFoundInConfig(answer.key, questionConfig.id))
  } yield getLocalizedLabel(labeledValue.label, language)

  private[this] def getMultiAnswerTextFromConfig(
    questionConfig: MultiQuestion,
    answer: RiskAnalysisMultiAnswer,
    language: Language
  ): Try[String] =
    questionConfig match {
      case question: MultiQuestion =>
        answer.values
          .traverse(answerValue =>
            question.options
              .find(_.value == answerValue)
              .toTry(AnswerNotFoundInConfig(answer.key, question.id))
              .map(labeledValue => getLocalizedLabel(labeledValue.label, language))
          )
          .map(_.mkString(", "))
    }

  private[this] def getLocalizedLabel(text: LocalizedText, language: Language): String =
    language match {
      case LanguageIt => text.it
      case LanguageEn => text.en
    }

  private[this] def answerToHtml(questionLabel: String, questionInfoLabel: Option[String], answer: String): String =
    s"""
       |<div class="item">
       |  <div class="label">$questionLabel</div>
       |  ${questionInfoLabel.fold("")(l => s"""<div class="info-label">$l</div>""")}
       |  <div class="answer">$answer</div>
       |</div>
       |""".stripMargin

  private[this] def createTempFile: Try[File] =
    Try {
      val fileTimestamp: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
      File.createTempFile(s"${fileTimestamp}_${UUID.randomUUID().toString}_risk_analysis.", ".pdf")
    }

}
