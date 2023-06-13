package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

final case class HideOptionConfig(id: String, value: String)

object HideOptionConfig extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[HideOptionConfig] = jsonFormat2(HideOptionConfig.apply)
}
