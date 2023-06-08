package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

final case class ValidationOption(maxLength: Option[Int] = None)

object ValidationOption extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[ValidationOption] = jsonFormat1(ValidationOption.apply)
}
