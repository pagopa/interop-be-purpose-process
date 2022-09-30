package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

final case class LabeledValue(pdfLabel: LocalizedText, value: String)

object LabeledValue extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[LabeledValue] = jsonFormat2(LabeledValue.apply)
}
