package it.pagopa.interop.purposeprocess.service.impl

import cats.implicits.toTraverseOps
import com.openhtmltopdf.util.XRLog
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
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.{Failure, Try}

object PDFCreatorImpl extends PDFCreator with PDFManager {

  // Suppressing openhtmltopdf log
  XRLog.listRegisteredLoggers.asScala.foreach((logger: String) =>
    XRLog.setLevel(logger, java.util.logging.Level.SEVERE)
  )

  private[this] val riskAnalysisForms: Map[String, RiskAnalysisFormConfig] = Map(
    "1.0" -> Source
      .fromResource("riskAnalysisTemplate/forms/1.0.json") // TODO Load all files in the folder
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]
  )

  override def createDocument(
    template: String,
    riskAnalysisForm: RiskAnalysisForm,
    dailyCalls: Int,
    language: Language
  ): Future[File] =
    Future.fromTry {
      for {
        file       <- createTempFile
        formConfig <- riskAnalysisForms
          .get(riskAnalysisForm.version)
          .toTry(FormTemplateConfigNotFound(riskAnalysisForm.version))
        data       <- setupData(formConfig, riskAnalysisForm, dailyCalls, language)
        pdf        <- getPDFAsFile(file.toPath, template, data)
      } yield pdf
    }

  private def createTempFile: Try[File] =
    Try {
      val fileTimestamp: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
      File.createTempFile(s"${fileTimestamp}_${UUID.randomUUID().toString}_risk_analysis.", ".pdf")
    }

  private def setupData(
    formConfig: RiskAnalysisFormConfig,
    riskAnalysisForm: RiskAnalysisForm,
    dailyCalls: Int,
    language: Language
  ): Try[Map[String, String]] =
    for {
      singleAnswers <- riskAnalysisForm.singleAnswers.traverse(formatSingleAnswer(formConfig, language))
      multiAnswers  <- riskAnalysisForm.multiAnswers.traverse(formatMultiAnswer(formConfig, language))
    } yield Map(
      "dailyCalls" -> dailyCalls.toString,
      "answers"    ->
        s"""
           | ${singleAnswers.mkString("\n")}
           | ${multiAnswers.mkString("\n")}
        """.stripMargin
    )

  private def formatSingleAnswer(formConfig: RiskAnalysisFormConfig, language: Language)(
    answer: RiskAnalysisSingleAnswer
  ): Try[String] =
    formatAnswer(formConfig, language, answer, answer.key, getSingleAnswerText)

  private def formatMultiAnswer(formConfig: RiskAnalysisFormConfig, language: Language)(
    answer: RiskAnalysisMultiAnswer
  ): Try[String] =
    formatAnswer(formConfig, language, answer, answer.key, getMultiAnswerText(_, _, language))

  private def formatAnswer[T](
    formConfig: RiskAnalysisFormConfig,
    language: Language,
    answer: T,
    answerKey: String,
    getText: (FormConfigQuestion, T) => Try[String]
  ): Try[String] =
    for {
      questionConfig <- getQuestionConfig(formConfig, answerKey)
      questionLabel = getLocalizedLabel(questionConfig.label, language)
      infoLabel     = questionConfig.infoLabel.map(getLocalizedLabel(_, language))
      answerText <- getText(questionConfig, answer)
    } yield answerToHtml(questionLabel, infoLabel, answerText)

  private def getSingleAnswerText(questionConfig: FormConfigQuestion, answer: RiskAnalysisSingleAnswer): Try[String] =
    questionConfig match {
      case _: SingleAnswerQuestionConfig => getSingleAnswerTextFromConfig(answer)
      case c: MultiAnswerQuestionConfig  => Failure(IncompatibleConfig(answer.key, c.id))
    }

  private def getMultiAnswerText(
    questionConfig: FormConfigQuestion,
    answer: RiskAnalysisMultiAnswer,
    language: Language
  ): Try[String] =
    questionConfig match {
      case c: SingleAnswerQuestionConfig => Failure(IncompatibleConfig(answer.key, c.id))
      case c: MultiAnswerQuestionConfig  => getMultiAnswerTextFromConfig(c, answer, language)
    }

  private def getSingleAnswerTextFromConfig(answer: RiskAnalysisSingleAnswer): Try[String] =
    answer.value.toTry(UnexpectedEmptyAnswer(answer.key))

  private def getMultiAnswerTextFromConfig(
    questionConfig: MultiAnswerQuestionConfig,
    answer: RiskAnalysisMultiAnswer,
    language: Language
  ): Try[String] =
    questionConfig match {
      case q: CheckboxQuestion =>
        answer.values
          .traverse(v =>
            q.options
              .find(_.value == v)
              .toTry(AnswerNotFoundInConfig(answer.key, q.id))
              .map(labeledValue => getLocalizedLabel(labeledValue.label, language))
          )
          .map(_.mkString(" ")) // TODO Check this
    }

  private def getLocalizedLabel(text: LocalizedText, language: Language): String =
    language match {
      case LanguageIt => text.it
      case LanguageEn => text.en
    }

  private def getQuestionConfig(formConfig: RiskAnalysisFormConfig, answerKey: String): Try[FormConfigQuestion] =
    formConfig.questions
      .find(_.id == answerKey)
      .toTry(QuestionNotFoundInConfig(answerKey, formConfig.version))

  private def answerToHtml(questionLabel: String, questionInfoLabel: Option[String], answer: String): String =
    s"""
       |<div class="item">
       |  <div class="label">$questionLabel</div>
       |  ${questionInfoLabel.fold("")(l => s"""<div class="info-label">$l</div>""")}
       |  <div class="answer">$answer</div>
       |</div>
       |""".stripMargin
}
