package it.pagopa.interop.purposeprocess.common.readmodel

import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

final case class ReadModelId(id: String)

object ReadModelId {
  implicit val rmiFormat: RootJsonFormat[ReadModelId] = jsonFormat1(ReadModelId.apply)
}
