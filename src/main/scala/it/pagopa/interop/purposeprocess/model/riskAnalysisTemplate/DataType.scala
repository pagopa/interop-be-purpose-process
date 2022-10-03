package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsString, JsValue, RootJsonFormat, deserializationError}

sealed trait DataType
object Single   extends DataType
object Multi    extends DataType
object FreeText extends DataType

object DataType extends DefaultJsonProtocol with SprayJsonSupport {
  implicit object EServiceDescriptorStateFormat extends RootJsonFormat[DataType] {
    def write(obj: DataType): JsValue =
      obj match {
        case Single   => JsString("single")
        case Multi    => JsString("multi")
        case FreeText => JsString("freeText")
      }

    def read(json: JsValue): DataType =
      json match {
        case JsString("single")   => Single
        case JsString("multi")    => Multi
        case JsString("freeText") => FreeText
        case unrecognized         =>
          deserializationError(s"DataType serialization error ${unrecognized.toString}")
      }
  }
}
