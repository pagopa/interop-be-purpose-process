package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

final case class LocalizedText(it: String, en: String)

object LocalizedText extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[LocalizedText] = jsonFormat2(LocalizedText.apply)
}
