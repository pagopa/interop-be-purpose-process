package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

final case class RiskAnalysisFormConfig(version: String, questions: List[FormConfigQuestion])

object RiskAnalysisFormConfig extends DefaultJsonProtocol with SprayJsonSupport {
  implicit def format: RootJsonFormat[RiskAnalysisFormConfig] = jsonFormat2(RiskAnalysisFormConfig.apply)

}
