package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiMarshaller
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import spray.json._

object PurposeApiMarshallerImpl extends PurposeApiMarshaller with SprayJsonSupport with DefaultJsonProtocol {

  override implicit def toEntityMarshallerPurpose: ToEntityMarshaller[Purpose] = sprayJsonMarshaller[Purpose]

  override implicit def fromEntityUnmarshallerPurposeSeed: FromEntityUnmarshaller[PurposeSeed] =
    sprayJsonUnmarshaller[PurposeSeed]

  override implicit def toEntityMarshallerProblem: ToEntityMarshaller[Problem] = sprayJsonMarshaller[Problem]

  override implicit def toEntityMarshallerPurposes: ToEntityMarshaller[Purposes] = sprayJsonMarshaller[Purposes]

  override implicit def fromEntityUnmarshallerPurposeVersionSeed: FromEntityUnmarshaller[PurposeVersionSeed] =
    sprayJsonUnmarshaller[PurposeVersionSeed]

  override implicit def toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion] =
    sprayJsonMarshaller[PurposeVersion]
}
