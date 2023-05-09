package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import it.pagopa.interop.purposeprocess.error.RiskAnalysisTemplateErrors.UnexpectedQuestionType
import spray.json._

sealed trait FormConfigQuestion {
  def id: String
  def label: LocalizedText
  def infoLabel: Option[LocalizedText]
  def dataType: DataType
  def required: Boolean
  def dependencies: List[Dependency]
  def `type`: String
  def defaultValue: List[String]
}

object FormConfigQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit object FormConfigQuestionJsonFormat extends RootJsonFormat[FormConfigQuestion] {
    def write(a: FormConfigQuestion): JsValue = a match {
      case q: FreeInputQuestion => q.toJson
      case q: SingleQuestion    => q.toJson
      case q: MultiQuestion     => q.toJson
    }

    def read(value: JsValue): FormConfigQuestion =
      value.asJsObject.fields("dataType") match {
        case JsString("freeText") => value.convertTo[FreeInputQuestion]
        case JsString("multi")    => value.convertTo[MultiQuestion]
        case JsString("single")   => value.convertTo[SingleQuestion]
        case other                => throw UnexpectedQuestionType(other.compactPrint)
      }
  }

  implicit val format: RootJsonFormat[FormConfigQuestion] = FormConfigQuestionJsonFormat
}

final case class FreeInputQuestion(
  id: String,
  label: LocalizedText,
  infoLabel: Option[LocalizedText],
  dataType: DataType,
  required: Boolean,
  dependencies: List[Dependency],
  `type`: String,
  defaultValue: List[String]
) extends FormConfigQuestion

object FreeInputQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[FreeInputQuestion] = jsonFormat8(FreeInputQuestion.apply)
}

final case class SingleQuestion(
  id: String,
  label: LocalizedText,
  infoLabel: Option[LocalizedText],
  dataType: DataType,
  required: Boolean,
  dependencies: List[Dependency],
  `type`: String,
  defaultValue: List[String],
  options: List[LabeledValue]
) extends FormConfigQuestion

object SingleQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[SingleQuestion] = jsonFormat9(SingleQuestion.apply)
}

final case class MultiQuestion(
  id: String,
  label: LocalizedText,
  infoLabel: Option[LocalizedText],
  dataType: DataType,
  required: Boolean,
  dependencies: List[Dependency],
  `type`: String,
  defaultValue: List[String],
  options: List[LabeledValue]
) extends FormConfigQuestion

object MultiQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[MultiQuestion] = jsonFormat9(MultiQuestion.apply)
}
