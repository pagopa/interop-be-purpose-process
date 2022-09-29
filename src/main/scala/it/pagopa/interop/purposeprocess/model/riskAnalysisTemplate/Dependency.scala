package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

final case class Dependency(id: String, value: String)

object Dependency extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[Dependency] = jsonFormat2(Dependency.apply)
}
