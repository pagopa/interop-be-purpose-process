package it.pagopa.interop.purposeprocess

import it.pagopa.interop.purposemanagement.model.purpose.{
  PersistentRiskAnalysisForm,
  PersistentRiskAnalysisMultiAnswer,
  PersistentRiskAnalysisSingleAnswer
}
import it.pagopa.interop.commons.riskanalysis.error.RiskAnalysisTemplateErrors._
import it.pagopa.interop.commons.riskanalysis.model.riskAnalysisTemplate._
import it.pagopa.interop.commons.riskanalysis.service.RiskAnalysisService
import it.pagopa.interop.purposeprocess.service.impl.PDFCreatorImpl.setupData
import it.pagopa.interop.purposeprocess.model.EServiceInfo
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.util.{Failure, Success, Try}

class PDFCreatorSpec extends AnyWordSpecLike with SpecHelper {

  import PDFCreatorSpec._

  val testConfig: RiskAnalysisFormConfig =
    RiskAnalysisService.loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/test.json")

  val languages = List(LanguageIt, LanguageEn)

  "Risk Analysis PDF creation" should {
    "succeed for 'freeText' type config" in {
      val questionKey      = "purpose"
      val answer           = "My Purpose"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)

      languages.foreach(language =>
        checkSuccessfulSingleAnswerResult(
          result = setupData(
            testConfig,
            riskAnalysisForm,
            dailyCalls,
            eServiceInfo,
            isFreeOfCharge,
            freeOfChargeReason,
            language
          ),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answer,
          language = language
        )
      )
    }

    "succeed for 'single' type config" in {
      val questionKey      = "usesPersonalData"
      val answer           = "YES"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)

      languages.foreach(language =>
        checkSuccessfulSingleAnswerResult(
          result = setupData(
            testConfig,
            riskAnalysisForm,
            dailyCalls,
            eServiceInfo,
            isFreeOfCharge,
            freeOfChargeReason,
            language
          ),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answer,
          language = language
        )
      )
    }

    "succeed for 'multi' type config" in {
      val questionKey      = "legalBasis"
      val answers          = List("CONSENT", "CONTRACT", "SAFEGUARD")
      val riskAnalysisForm = makeMultiAnswerForm(key = questionKey, values = answers)

      languages.foreach(language =>
        checkSuccessfulResult(
          result = setupData(
            testConfig,
            riskAnalysisForm,
            dailyCalls,
            eServiceInfo,
            isFreeOfCharge,
            freeOfChargeReason,
            language
          ),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answers,
          language = language
        )
      )
    }

    "succeed without free of charge reason" in {
      val questionKey      = "legalBasis"
      val answers          = List("CONSENT", "CONTRACT", "SAFEGUARD")
      val riskAnalysisForm = makeMultiAnswerForm(key = questionKey, values = answers)

      languages.foreach(language =>
        checkSuccessfulResult(
          result = setupData(
            testConfig,
            riskAnalysisForm,
            dailyCalls,
            eServiceInfo,
            isFreeOfCharge = false,
            freeOfChargeReason = None,
            language
          ),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answers,
          language = language
        )
      )
    }

    "succeed with free of charge but without reason" in {
      val questionKey      = "legalBasis"
      val answers          = List("CONSENT", "CONTRACT", "SAFEGUARD")
      val riskAnalysisForm = makeMultiAnswerForm(key = questionKey, values = answers)

      languages.foreach(language =>
        checkSuccessfulResult(
          result = setupData(
            testConfig,
            riskAnalysisForm,
            dailyCalls,
            eServiceInfo,
            isFreeOfCharge = true,
            freeOfChargeReason = None,
            language
          ),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answers,
          language = language
        )
      )
    }

    "succeed with free of charge reason" in {
      val questionKey      = "legalBasis"
      val answers          = List("CONSENT", "CONTRACT", "SAFEGUARD")
      val riskAnalysisForm = makeMultiAnswerForm(key = questionKey, values = answers)

      languages.foreach(language =>
        checkSuccessfulResult(
          result = setupData(
            testConfig,
            riskAnalysisForm,
            dailyCalls,
            eServiceInfo,
            isFreeOfCharge = true,
            freeOfChargeReason = Some("Reason"),
            language
          ),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answers,
          language = language
        )
      )
    }

    "keep the template questions sorting" in {
      val freeQuestionKey = "purpose"
      val freeAnswer      = "My Purpose"
      val freeLabel       =
        answerHtmlLabel(testConfig.questions.find(_.id == freeQuestionKey).get, List(freeAnswer), _.label.it)
      val freeForm        = makeSingleAnswerForm(key = freeQuestionKey, value = freeAnswer)

      val singleQuestionKey1 = "usesPersonalData"
      val singleAnswer1      = "YES"
      val singleLabel1       =
        answerHtmlLabel(testConfig.questions.find(_.id == singleQuestionKey1).get, List(singleAnswer1), _.label.it)
      val singleForm1        = makeSingleAnswerForm(key = singleQuestionKey1, value = singleAnswer1)

      val multiQuestionKey = "legalBasis"
      val multiAnswers     = List("CONSENT", "CONTRACT", "SAFEGUARD")
      val multiLabel       =
        answerHtmlLabel(testConfig.questions.find(_.id == multiQuestionKey).get, multiAnswers, _.label.it)
      val multiForm        = makeMultiAnswerForm(key = multiQuestionKey, values = multiAnswers)

      val singleQuestionKey2 = "dataQuantity"
      val singleAnswer2      = "QUANTITY_101_TO_500"
      val singleLabel2       =
        answerHtmlLabel(testConfig.questions.find(_.id == singleQuestionKey2).get, List(singleAnswer2), _.label.it)
      val singleForm2        = makeSingleAnswerForm(key = singleQuestionKey2, value = singleAnswer2)

      val riskAnalysisForm = dummyRiskAnalysisForm.copy(
        singleAnswers = freeForm.singleAnswers ++ singleForm1.singleAnswers ++ singleForm2.singleAnswers,
        multiAnswers = multiForm.multiAnswers
      )

      val result = setupData(
        testConfig,
        riskAnalysisForm,
        dailyCalls,
        eServiceInfo,
        isFreeOfCharge,
        freeOfChargeReason,
        LanguageIt
      )

      result shouldBe a[Success[_]]
      val answers = result.get("answers")

      answers.indexOf(freeLabel) should be > -1
      answers.indexOf(freeLabel) should be < answers.indexOf(singleLabel1)
      answers.indexOf(singleLabel1) should be < answers.indexOf(multiLabel)
      answers.indexOf(multiLabel) should be < answers.indexOf(singleLabel2)
    }

    "fail if the question is not of the config expected type" in {
      val questionKey      = "usesPersonalData"
      val answer           = "YES"
      val riskAnalysisForm = makeMultiAnswerForm(key = questionKey, values = List(answer))
      val result           = setupData(
        testConfig,
        riskAnalysisForm,
        dailyCalls,
        eServiceInfo,
        isFreeOfCharge,
        freeOfChargeReason,
        LanguageIt
      )

      result.fold(_ shouldBe a[IncompatibleConfig], _ => fail("Expected failure, but got Success"))
    }

    "fail if the answer option is not in config" in {
      val questionKey      = "usesPersonalData"
      val answer           = "non-existent-answer"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)

      val result = setupData(
        testConfig,
        riskAnalysisForm,
        dailyCalls,
        eServiceInfo,
        isFreeOfCharge,
        freeOfChargeReason,
        LanguageIt
      )
      result.fold(_ shouldBe a[AnswerNotFoundInConfig], _ => fail("Expected failure, but got Success"))
    }

    "fail if the single answer is empty" in {
      val questionKey      = "usesPersonalData"
      val riskAnalysisForm = dummyRiskAnalysisForm.copy(singleAnswers =
        Seq(PersistentRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = questionKey, value = None))
      )

      val result = setupData(
        testConfig,
        riskAnalysisForm,
        dailyCalls,
        eServiceInfo,
        isFreeOfCharge,
        freeOfChargeReason,
        LanguageIt
      )
      result.fold(_ shouldBe a[UnexpectedEmptyAnswer], _ => fail("Expected failure, but got Success"))
    }
  }

}

object PDFCreatorSpec {
  val eServiceInfo: EServiceInfo                        =
    EServiceInfo(
      "EServiceName",
      "ProducerName",
      "ProducerOrigin",
      "ProducerIPACode",
      "ConsumerName",
      "ConsumerOrigin",
      "consumerIPACode"
    )
  val isFreeOfCharge: Boolean                           = true
  val freeOfChargeReason: Option[String]                = Some("Reason")
  val dailyCalls                                        = 1000
  val dummyRiskAnalysisForm: PersistentRiskAnalysisForm =
    PersistentRiskAnalysisForm(
      id = UUID.randomUUID(),
      riskAnalysisId = Some(UUID.randomUUID()),
      version = "1.0",
      singleAnswers = Nil,
      multiAnswers = Nil
    )

  def makeSingleAnswerForm(key: String, value: String): PersistentRiskAnalysisForm =
    dummyRiskAnalysisForm.copy(singleAnswers =
      Seq(PersistentRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = key, value = Some(value)))
    )

  def makeMultiAnswerForm(key: String, values: List[String]): PersistentRiskAnalysisForm =
    dummyRiskAnalysisForm.copy(multiAnswers =
      Seq(PersistentRiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = key, values = values))
    )

  def checkSuccessfulSingleAnswerResult(
    result: Try[Map[String, String]],
    expectedQuestion: FormConfigQuestion,
    expectedAnswer: String,
    language: Language
  ): Assertion =
    checkSuccessfulResult(result, expectedQuestion, List(expectedAnswer), language)

  def checkSuccessfulResult(
    result: Try[Map[String, String]],
    expectedQuestion: FormConfigQuestion,
    expectedAnswer: List[String],
    language: Language
  ): Assertion =
    result match {
      case Success(value) =>
        value should contain("dailyCalls" -> dailyCalls.toString)
        value should contain("eServiceName" -> eServiceInfo.name)
        value.get("freeOfCharge") should not be empty
        value.get("freeOfChargeReason") should not be empty
        value.get("producerText") should not be empty
        value.get("consumerText") should not be empty
        value.get("date") should not be empty

        value.get("answers") should not be empty
        val answers = value("answers")

        def checkAnswer(getValue: LabeledValue => String): Assertion = {
          val labels = answerHtmlLabel(expectedQuestion, expectedAnswer, getValue)
          answers should include(labels)
        }

        language match {
          case LanguageIt =>
            answers should include(expectedQuestion.label.it)
            expectedQuestion.infoLabel.fold(succeed)(l => answers should include(l.it))
            checkAnswer(_.label.it)
          case LanguageEn =>
            answers should include(expectedQuestion.label.en)
            expectedQuestion.infoLabel.fold(succeed)(l => answers should include(l.en))
            checkAnswer(_.label.en)
        }

      case Failure(exception) => fail(exception)
    }

  def answerHtmlLabel(
    question: FormConfigQuestion,
    answers: List[String],
    localizedValue: LabeledValue => String
  ): String =
    question match {
      case _: FreeInputQuestion =>
        s"""<div class="answer">${answers.head}</div>"""
      case q: SingleQuestion    =>
        val a = q.options.find(_.value == answers.head).map(localizedValue).get
        s"""<div class="answer">$a</div>"""
      case q: MultiQuestion     =>
        val a = q.options.filter(o => answers.contains(o.value)).map(localizedValue)
        s"""<div class="answer">${a.mkString(", ")}</div>"""
    }

}
