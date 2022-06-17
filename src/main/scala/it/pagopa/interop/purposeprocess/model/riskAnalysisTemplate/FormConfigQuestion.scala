package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

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
      case q: RadioQuestion     => q.toJson
    }

    def read(value: JsValue): FormConfigQuestion =
      value.asJsObject.fields("type") match {
        case JsString("text")       => value.convertTo[FreeInputQuestion]
        case JsString("checkbox")   => value.convertTo[CheckboxQuestion]
        case JsString("radio")      => value.convertTo[RadioQuestion]
        case JsString("select-one") => value.convertTo[RadioQuestion] // This has the same behaviour of radio
        case v                      => throw new RuntimeException(s"Failed to decode $v")
      }
  }

  implicit val format: RootJsonFormat[FormConfigQuestion] = FormConfigQuestionJsonFormat
}

final case class FreeInputQuestion(id: String, label: LocalizedText, infoLabel: Option[LocalizedText], `type`: String)
    extends SingleAnswerQuestionConfig

object FreeInputQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[FreeInputQuestion] = jsonFormat4(FreeInputQuestion.apply)
}

final case class RadioQuestion(
  id: String,
  label: LocalizedText,
  infoLabel: Option[LocalizedText],
  `type`: String,
  options: Seq[LabeledValue]
) extends SingleAnswerQuestionConfig

object RadioQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[RadioQuestion] = jsonFormat5(RadioQuestion.apply)
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
