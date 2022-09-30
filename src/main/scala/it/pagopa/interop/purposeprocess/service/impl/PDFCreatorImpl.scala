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
import it.pagopa.interop.purposeprocess.service.PDFCreator
import spray.json._

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.Future
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Try}

object PDFCreatorImpl extends PDFCreator with PDFManager {

  // Suppressing openhtmltopdf log
  XRLog.listRegisteredLoggers.asScala.foreach((logger: String) =>
    XRLog.setLevel(logger, java.util.logging.Level.SEVERE)
  )

  private[this] val printedDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  private[this] val pdfConfigs: PDFConfiguration = PDFConfiguration(resourcesBaseUrl = Some("/riskAnalysisTemplate/"))

  // The Map key must correspond to the version field of the risk analysis form
  private[this] val riskAnalysisForms: Map[String, RiskAnalysisFormConfig] = Map(
    "1.0" -> loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/1.0.json")
    // TODO Add 2.0
  )

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
        formConfig <- riskAnalysisForms
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
      singleAnswers <- riskAnalysisForm.singleAnswers.traverse(formatSingleAnswer(formConfig, language))
      multiAnswers  <- riskAnalysisForm.multiAnswers.traverse(formatMultiAnswer(formConfig, language))
    } yield Map(
      "dailyCalls"   -> dailyCalls.toString,
      "answers"      ->
        s"""
           | ${singleAnswers.mkString("\n")}
           | ${multiAnswers.mkString("\n")}
        """.stripMargin,
      "eServiceName" -> eServiceInfo.name,
      "producerName" -> eServiceInfo.producerName,
      "consumerName" -> eServiceInfo.consumerName,
      "date"         -> LocalDateTime.now().format(printedDateFormatter)
    )

  private[this] def formatSingleAnswer(formConfig: RiskAnalysisFormConfig, language: Language)(
    answer: RiskAnalysisSingleAnswer
  ): Try[String] =
    formatAnswer(formConfig, language, answer, answer.key, getSingleAnswerText(language))

  private[this] def formatMultiAnswer(formConfig: RiskAnalysisFormConfig, language: Language)(
    answer: RiskAnalysisMultiAnswer
  ): Try[String] =
    formatAnswer(formConfig, language, answer, answer.key, getMultiAnswerText(language))

  private[this] def formatAnswer[T](
    formConfig: RiskAnalysisFormConfig,
    language: Language,
    answer: T,
    answerKey: String,
    getText: (FormConfigQuestion, T) => Try[String]
  ): Try[String] =
    for {
      questionConfig <- getQuestionConfig(formConfig, answerKey)
      questionLabel = getLocalizedLabel(questionConfig.pdfLabel, language)
      infoLabel     = questionConfig.pdfInfoLabel.map(getLocalizedLabel(_, language))
      answerText <- getText(questionConfig, answer)
    } yield answerToHtml(questionLabel, infoLabel, answerText)

  private[this] def getSingleAnswerText(
    language: Language
  )(questionConfig: FormConfigQuestion, answer: RiskAnalysisSingleAnswer): Try[String] =
    questionConfig match {
      case c: SingleAnswerQuestionConfig => getSingleAnswerTextFromConfig(c, answer, language)
      case c: MultiAnswerQuestionConfig  => Failure(IncompatibleConfig(answer.key, c.id))
    }

  private[this] def getMultiAnswerText(
    language: Language
  )(questionConfig: FormConfigQuestion, answer: RiskAnalysisMultiAnswer): Try[String] =
    questionConfig match {
      case c: SingleAnswerQuestionConfig => Failure(IncompatibleConfig(answer.key, c.id))
      case c: MultiAnswerQuestionConfig  => getMultiAnswerTextFromConfig(c, answer, language)
    }

  private[this] def getSingleAnswerTextFromConfig(
    questionConfig: SingleAnswerQuestionConfig,
    answer: RiskAnalysisSingleAnswer,
    language: Language
  ): Try[String] =
    questionConfig match {
      case _: FreeInputQuestion     => answer.value.toTry(UnexpectedEmptyAnswer(answer.key))
      case question: SingleQuestion =>
        for {
          answerValue  <- answer.value.toTry(UnexpectedEmptyAnswer(answer.key))
          labeledValue <- question.options
            .find(_.value == answerValue)
            .toTry(AnswerNotFoundInConfig(answer.key, question.id))
        } yield getLocalizedLabel(labeledValue.pdfLabel, language)
    }

  private[this] def getMultiAnswerTextFromConfig(
    questionConfig: MultiAnswerQuestionConfig,
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
              .map(labeledValue => getLocalizedLabel(labeledValue.pdfLabel, language))
          )
          .map(_.mkString(", "))
    }

  private[this] def getLocalizedLabel(text: LocalizedText, language: Language): String =
    language match {
      case LanguageIt => text.it
      case LanguageEn => text.en
    }

  private[this] def getQuestionConfig(formConfig: RiskAnalysisFormConfig, answerKey: String): Try[FormConfigQuestion] =
    formConfig.questions
      .find(_.id == answerKey)
      .toTry(QuestionNotFoundInConfig(answerKey, formConfig.version))

  private[this] def answerToHtml(questionLabel: String, questionInfoLabel: Option[String], answer: String): String =
    s"""
       |<div class="item">
       |  <div class="label">$questionLabel</div>
       |  ${questionInfoLabel.fold("")(l => s"""<div class="info-label">$l</div>""")}
       |  <div class="answer">$answer</div>
       |</div>
       |""".stripMargin

  private[this] def loadRiskAnalysisFormConfig(resourcePath: String) =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]

  private[this] def createTempFile: Try[File] =
    Try {
      val fileTimestamp: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
      File.createTempFile(s"${fileTimestamp}_${UUID.randomUUID().toString}_risk_analysis.", ".pdf")
    }

}
