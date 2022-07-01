package it.pagopa.interop.purposeprocess

import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisForm,
  RiskAnalysisMultiAnswer,
  RiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.error.RiskAnalysisTemplateErrors._
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate._
import it.pagopa.interop.purposeprocess.service.impl.PDFCreatorImpl.setupData
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json._

import java.util.UUID
import scala.io.Source
import scala.util.{Failure, Success, Try}

class PDFCreatorSpec extends AnyWordSpecLike with SpecHelper {

  import PDFCreatorSpec._

  val testConfig: RiskAnalysisFormConfig = loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/test.json")

  val languages = List(LanguageIt, LanguageEn)

  "Risk Analysis PDF creation" should {
    "succeed for 'text' type config" in {
      val questionKey      = "purpose"
      val answer           = "My Purpose"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)

      languages.foreach(language =>
        checkSuccessfulSingleAnswerResult(
          result = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, language),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answer,
          language = language
        )
      )
    }

    "succeed for 'radio' type config" in {
      val questionKey      = "usesPersonalData"
      val answer           = "YES"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)

      languages.foreach(language =>
        checkSuccessfulSingleAnswerResult(
          result = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, language),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answer,
          language = language
        )
      )
    }

    "succeed for 'checkbox' type config" in {
      val questionKey      = "legalBasis"
      val answers          = List("CONSENT", "CONTRACT", "SAFEGUARD")
      val riskAnalysisForm = makeMultiAnswerForm(key = questionKey, values = answers)

      languages.foreach(language =>
        checkSuccessfulResult(
          result = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, language),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answers,
          language = language
        )
      )
    }

    "succeed for 'select-one' type config" in {
      val questionKey      = "dataQuantity"
      val answer           = "QUANTITY_0_TO_100"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)

      languages.foreach(language =>
        checkSuccessfulSingleAnswerResult(
          result = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, language),
          expectedQuestion = testConfig.questions.find(_.id == questionKey).get,
          expectedAnswer = answer,
          language = language
        )
      )
    }

    "fail if the question is not in config" in {
      val questionKey      = "non-existent-key"
      val answer           = "someValue"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)
      val result           = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, LanguageIt)

      result.fold(_ shouldBe a[QuestionNotFoundInConfig], _ => fail("Expected failure, but got Success"))
    }

    "fail if the question is not of the config expected type" in {
      val questionKey      = "usesPersonalData"
      val answer           = "YES"
      val riskAnalysisForm = makeMultiAnswerForm(key = questionKey, values = List(answer))
      val result           = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, LanguageIt)

      result.fold(_ shouldBe a[IncompatibleConfig], _ => fail("Expected failure, but got Success"))
    }

    "fail if the answer option is not in config" in {
      val questionKey      = "usesPersonalData"
      val answer           = "non-existent-answer"
      val riskAnalysisForm = makeSingleAnswerForm(key = questionKey, value = answer)

      val result = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, LanguageIt)
      result.fold(_ shouldBe a[AnswerNotFoundInConfig], _ => fail("Expected failure, but got Success"))
    }

    "fail if the single answer is empty" in {
      val questionKey      = "usesPersonalData"
      val riskAnalysisForm = dummyRiskAnalysisForm.copy(singleAnswers =
        Seq(RiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = questionKey, value = None))
      )

      val result = setupData(testConfig, riskAnalysisForm, dailyCalls, eServiceInfo, LanguageIt)
      result.fold(_ shouldBe a[UnexpectedEmptyAnswer], _ => fail("Expected failure, but got Success"))
    }
  }

}

object PDFCreatorSpec {
  val eServiceInfo: EServiceInfo              = EServiceInfo("EServiceName", "ProducerName", "ConsumerName")
  val dailyCalls                              = 1000
  val dummyRiskAnalysisForm: RiskAnalysisForm =
    RiskAnalysisForm(id = UUID.randomUUID(), version = "1.0", singleAnswers = Nil, multiAnswers = Nil)

  def makeSingleAnswerForm(key: String, value: String): RiskAnalysisForm =
    dummyRiskAnalysisForm.copy(singleAnswers =
      Seq(RiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = key, value = Some(value)))
    )

  def makeMultiAnswerForm(key: String, values: List[String]): RiskAnalysisForm =
    dummyRiskAnalysisForm.copy(multiAnswers =
      Seq(RiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = key, values = values))
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
        value should contain("producerName" -> eServiceInfo.producerName)
        value should contain("consumerName" -> eServiceInfo.consumerName)
        value.get("date") should not be empty

        value.get("answers") should not be empty
        val answers = value("answers")

        def checkAnswer(getValue: LabeledValue => String): Assertion =
          expectedQuestion match {
            case _: FreeInputQuestion =>
              expectedAnswer.size shouldBe 1
              answers should include(expectedAnswer.head)
            case q: RadioQuestion     =>
              expectedAnswer.size shouldBe 1
              answers should include(q.options.find(_.value == expectedAnswer.head).map(getValue).get)
            case q: CheckboxQuestion  =>
              answers should expectedAnswer
                .map(a => include(q.options.find(_.value == a).map(getValue).get))
                .reduce(_ and _)
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

  def loadRiskAnalysisFormConfig(resourcePath: String): RiskAnalysisFormConfig =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]
}
