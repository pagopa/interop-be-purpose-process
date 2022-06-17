package it.pagopa.interop.purposeprocess.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

final case class RiskAnalysisFormConfig(version: String, questions: List[FormConfigQuestion])

object RiskAnalysisFormConfig extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[RiskAnalysisFormConfig] = jsonFormat2(RiskAnalysisFormConfig.apply)

}

sealed trait Language
object LanguageIt extends Language
object LanguageEn extends Language

final case class LocalizedText(it: String, en: String)

object LocalizedText extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[LocalizedText] = jsonFormat2(LocalizedText.apply)
}

sealed trait FormConfigQuestion {
  def id: String
  def label: LocalizedText
  def infoLabel: Option[LocalizedText]
  def `type`: String
}

sealed trait SingleAnswerQuestionConfig extends FormConfigQuestion
sealed trait MultiAnswerQuestionConfig  extends FormConfigQuestion

object FormConfigQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit object FormConfigQuestionJsonFormat extends RootJsonFormat[FormConfigQuestion] {
    def write(a: FormConfigQuestion): JsValue = a match {
      case q: FreeInputQuestion => q.toJson
      case q: CheckboxQuestion  => q.toJson
    }

    def read(value: JsValue): FormConfigQuestion =
      value.asJsObject.fields("type") match {
        case JsString("text")     => value.convertTo[FreeInputQuestion]
        case JsString("checkbox") => value.convertTo[CheckboxQuestion]
        case v                    => throw new RuntimeException(s"Failed to decode $v")
      }
  }

  implicit val format: RootJsonFormat[FormConfigQuestion] = FormConfigQuestionJsonFormat
}

final case class FreeInputQuestion(id: String, label: LocalizedText, infoLabel: Option[LocalizedText], `type`: String)
    extends SingleAnswerQuestionConfig

object FreeInputQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[FreeInputQuestion] = jsonFormat4(FreeInputQuestion.apply)
}

final case class CheckboxQuestion(
  id: String,
  label: LocalizedText,
  infoLabel: Option[LocalizedText],
  `type`: String,
  options: Seq[LabeledValue]
) extends MultiAnswerQuestionConfig

object CheckboxQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[CheckboxQuestion] = jsonFormat5(CheckboxQuestion.apply)
}

final case class LabeledValue(label: LocalizedText, value: String)

object LabeledValue extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[LabeledValue] = jsonFormat2(LabeledValue.apply)
}
