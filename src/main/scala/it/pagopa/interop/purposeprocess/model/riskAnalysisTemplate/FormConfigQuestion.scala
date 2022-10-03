package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import it.pagopa.interop.purposeprocess.error.RiskAnalysisTemplateErrors.UnexpectedQuestionType
import spray.json._

sealed trait FormConfigQuestion {
  def id: String
  def pdfLabel: LocalizedText
  def pdfInfoLabel: Option[LocalizedText]
  def dataType: String
  def required: Boolean
  def dependencies: List[Dependency]
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
  pdfLabel: LocalizedText,
  pdfInfoLabel: Option[LocalizedText],
  dataType: String,
  required: Boolean,
  dependencies: List[Dependency]
) extends FormConfigQuestion

object FreeInputQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[FreeInputQuestion] = jsonFormat6(FreeInputQuestion.apply)
}

final case class SingleQuestion(
  id: String,
  pdfLabel: LocalizedText,
  pdfInfoLabel: Option[LocalizedText],
  dataType: String,
  options: List[LabeledValue],
  required: Boolean,
  dependencies: List[Dependency]
) extends FormConfigQuestion

object SingleQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[SingleQuestion] = jsonFormat7(SingleQuestion.apply)
}

final case class MultiQuestion(
  id: String,
  pdfLabel: LocalizedText,
  pdfInfoLabel: Option[LocalizedText],
  dataType: String,
  options: List[LabeledValue],
  required: Boolean,
  dependencies: List[Dependency]
) extends FormConfigQuestion

object MultiQuestion extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[MultiQuestion] = jsonFormat7(MultiQuestion.apply)
}
