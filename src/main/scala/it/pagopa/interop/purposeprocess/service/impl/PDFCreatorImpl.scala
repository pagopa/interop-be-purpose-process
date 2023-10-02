package it.pagopa.interop.purposeprocess.service.impl

import cats.implicits.toTraverseOps
import com.openhtmltopdf.util.XRLog
import it.pagopa.interop.commons.files.model.PDFConfiguration
import it.pagopa.interop.commons.files.service.PDFManager
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.purposemanagement.model.purpose.{
  PersistentRiskAnalysisForm,
  PersistentRiskAnalysisMultiAnswer,
  PersistentRiskAnalysisSingleAnswer
}
import it.pagopa.interop.commons.riskanalysis.error.RiskAnalysisTemplateErrors._
import it.pagopa.interop.commons.riskanalysis.model.riskAnalysisTemplate._
import it.pagopa.interop.commons.riskanalysis.service.RiskAnalysisService
import it.pagopa.interop.purposeprocess.service.PDFCreator

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Try}
import it.pagopa.interop.purposeprocess.model.EServiceInfo
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind
import it.pagopa.interop.purposeprocess.api.Adapters._
object PDFCreatorImpl extends PDFCreator with PDFManager {
  val YES           = "Si"
  val NO            = "No"
  val NOT_AVAILABLE = "N/A"
  // Suppressing openhtmltopdf log
  XRLog.listRegisteredLoggers.asScala.foreach((logger: String) =>
    XRLog.setLevel(logger, java.util.logging.Level.SEVERE)
  )

  private[this] val printedDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  private[this] val pdfConfigs: PDFConfiguration = PDFConfiguration(resourcesBaseUrl = Some("/riskAnalysisTemplate/"))

  override def createDocument(
    template: String,
    riskAnalysisForm: PersistentRiskAnalysisForm,
    dailyCalls: Int,
    eServiceInfo: EServiceInfo,
    isFreeOfCharge: Boolean,
    freeOfChargeReason: Option[String],
    language: Language
  )(kind: PersistentTenantKind): Future[File] =
    Future.fromTry {
      for {
        file       <- createTempFile
        kindConfig <- RiskAnalysisService
          .riskAnalysisForms()
          .get(kind.toTemplate)
          .toTry(TenantKindTemplateConfigNotFound(kind.toTemplate))
        formConfig <- kindConfig
          .get(riskAnalysisForm.version)
          .toTry(FormTemplateConfigNotFound(riskAnalysisForm.version))
        data       <- setupData(
          formConfig,
          riskAnalysisForm,
          dailyCalls,
          eServiceInfo,
          isFreeOfCharge,
          freeOfChargeReason,
          language
        )
        pdf        <- getPDFAsFileWithConfigs(file.toPath, template, data, pdfConfigs)
      } yield pdf
    }

  def setupData(
    formConfig: RiskAnalysisFormConfig,
    riskAnalysisForm: PersistentRiskAnalysisForm,
    dailyCalls: Int,
    eServiceInfo: EServiceInfo,
    isFreeOfCharge: Boolean,
    freeOfChargeReason: Option[String],
    language: Language
  ): Try[Map[String, String]] =
    for {
      answers <- sortedAnswers(formConfig, riskAnalysisForm, language)
      (freeOfChargeHtml, freeOfChargeReasonHtml) = formatFreeOfCharge(isFreeOfCharge, freeOfChargeReason)
    } yield Map(
      "dailyCalls"         -> dailyCalls.toString,
      "answers"            -> answers.mkString("\n"),
      "eServiceName"       -> eServiceInfo.name,
      "producerText"       -> getDescriptionText(
        eServiceInfo.producerName,
        eServiceInfo.producerOrigin,
        eServiceInfo.producerIPACode
      ),
      "consumerText"       -> getDescriptionText(
        eServiceInfo.consumerName,
        eServiceInfo.consumerOrigin,
        eServiceInfo.consumerIPACode
      ),
      "freeOfCharge"       -> freeOfChargeHtml,
      "freeOfChargeReason" -> freeOfChargeReasonHtml,
      "date"               -> LocalDateTime.now().format(printedDateFormatter)
    )

  def getDescriptionText(name: String, origin: String, value: String): String =
    if (origin == "IPA") s"$name (codice IPA: ${value})"
    else name

  def sortedAnswers(
    formConfig: RiskAnalysisFormConfig,
    riskAnalysisForm: PersistentRiskAnalysisForm,
    language: Language
  ): Try[List[String]] =
    formConfig.questions
      .flatMap(config =>
        riskAnalysisForm.singleAnswers.find(_.key == config.id).map(formatSingleAnswer(config, language)) orElse
          riskAnalysisForm.multiAnswers.find(_.key == config.id).map(formatMultiAnswer(config, language))
      )
      .sequence

  private[this] def formatSingleAnswer(formConfig: FormConfigQuestion, language: Language)(
    answer: PersistentRiskAnalysisSingleAnswer
  ): Try[String] = formatAnswer(formConfig, language, answer, getSingleAnswerText(language))

  private[this] def formatMultiAnswer(formConfig: FormConfigQuestion, language: Language)(
    answer: PersistentRiskAnalysisMultiAnswer
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
  )(questionConfig: FormConfigQuestion, answer: PersistentRiskAnalysisSingleAnswer): Try[String] =
    questionConfig match {
      case _: FreeInputQuestion => answer.value.toTry(UnexpectedEmptyAnswer(answer.key))
      case c: SingleQuestion    => getSingleAnswerTextFromConfig(c, answer, language)
      case c: MultiQuestion     => Failure(IncompatibleConfig(answer.key, c.id))
    }

  private[this] def getMultiAnswerText(
    language: Language
  )(questionConfig: FormConfigQuestion, answer: PersistentRiskAnalysisMultiAnswer): Try[String] =
    questionConfig match {
      case c: FreeInputQuestion => Failure(IncompatibleConfig(answer.key, c.id))
      case c: SingleQuestion    => Failure(IncompatibleConfig(answer.key, c.id))
      case c: MultiQuestion     => getMultiAnswerTextFromConfig(c, answer, language)
    }

  private[this] def getSingleAnswerTextFromConfig(
    questionConfig: SingleQuestion,
    answer: PersistentRiskAnalysisSingleAnswer,
    language: Language
  ): Try[String] = for {
    answerValue  <- answer.value.toTry(UnexpectedEmptyAnswer(answer.key))
    labeledValue <- questionConfig.options
      .find(_.value == answerValue)
      .toTry(AnswerNotFoundInConfig(answer.key, questionConfig.id))
  } yield getLocalizedLabel(labeledValue.label, language)

  private[this] def getMultiAnswerTextFromConfig(
    questionConfig: MultiQuestion,
    answer: PersistentRiskAnalysisMultiAnswer,
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

  private[this] def freeOfChargeToHtml(isFreeOfCharge: Boolean): String = {
    val yesOrNot = if (isFreeOfCharge) YES else NO
    s"""
       |<div class="item">
       |  <div class="label">Indicare se l'accesso ai dati messi a disposizione con la fruizione del presente e-service è a titolo gratuito</div>
       |  <div class="value">$yesOrNot</div>
       |</div>
       |""".stripMargin
  }

  private[this] def freeOfChargeReasonToHtml(isFreeOfCharge: Boolean, freeOfChargeReason: Option[String]): String = {
    val reason = freeOfChargeReason.getOrElse(NOT_AVAILABLE)
    if (isFreeOfCharge)
      s"""
         |<div class="item">
         |  <div class="label">Motivazione titolo gratuito</div>
         |  <div class="value">$reason</div>
         |</div>
         |""".stripMargin
    else """<div class="item-not-visible"></div>""".stripMargin
  }

  private[this] def formatFreeOfCharge(isFreeOfCharge: Boolean, freeOfChargeReason: Option[String]): (String, String) =
    (freeOfChargeToHtml(isFreeOfCharge), freeOfChargeReasonToHtml(isFreeOfCharge, freeOfChargeReason))

  private[this] def createTempFile: Try[File] =
    Try {
      val fileTimestamp: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
      File.createTempFile(s"${fileTimestamp}_${UUID.randomUUID().toString}_risk_analysis.", ".pdf")
    }

}
