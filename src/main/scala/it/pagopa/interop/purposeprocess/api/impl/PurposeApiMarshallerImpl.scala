package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import it.pagopa.interop.purposeprocess.api.PurposeApiMarshaller
import it.pagopa.interop.purposeprocess.model._
import spray.json._

object PurposeApiMarshallerImpl extends PurposeApiMarshaller with SprayJsonSupport with DefaultJsonProtocol {

  override implicit def toEntityMarshallerOldPurpose: ToEntityMarshaller[OldPurpose] = sprayJsonMarshaller[OldPurpose]

  override implicit def fromEntityUnmarshallerPurposeSeed: FromEntityUnmarshaller[PurposeSeed] =
    sprayJsonUnmarshaller[PurposeSeed]

  override implicit def toEntityMarshallerProblem: ToEntityMarshaller[Problem] = entityMarshallerProblem

  override implicit def toEntityMarshallerPurposes: ToEntityMarshaller[Purposes] = sprayJsonMarshaller[Purposes]

  override implicit def fromEntityUnmarshallerPurposeVersionSeed: FromEntityUnmarshaller[PurposeVersionSeed] =
    sprayJsonUnmarshaller[PurposeVersionSeed]

  override implicit def toEntityMarshallerPurposeVersion: ToEntityMarshaller[PurposeVersion] =
    sprayJsonMarshaller[PurposeVersion]

  override implicit def fromEntityUnmarshallerPurposeUpdateContent: FromEntityUnmarshaller[PurposeUpdateContent] =
    sprayJsonUnmarshaller[PurposeUpdateContent]

  override implicit def fromEntityUnmarshallerDraftPurposeVersionUpdateContent
    : FromEntityUnmarshaller[DraftPurposeVersionUpdateContent] = sprayJsonUnmarshaller[DraftPurposeVersionUpdateContent]

  override implicit def fromEntityUnmarshallerWaitingForApprovalPurposeVersionUpdateContent
    : FromEntityUnmarshaller[WaitingForApprovalPurposeVersionUpdateContent] =
    sprayJsonUnmarshaller[WaitingForApprovalPurposeVersionUpdateContent]

  override implicit def toEntityMarshallerPurposeVersionDocument: ToEntityMarshaller[PurposeVersionDocument] =
    sprayJsonMarshaller[PurposeVersionDocument]

}
