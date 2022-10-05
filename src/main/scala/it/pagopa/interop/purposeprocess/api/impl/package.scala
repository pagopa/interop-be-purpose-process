package it.pagopa.interop.purposeprocess.api

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.StatusCode
import it.pagopa.interop._
import it.pagopa.interop.commons.jwt.service.JWTReader
import it.pagopa.interop.commons.utils.AkkaUtils.getFutureBearer
import it.pagopa.interop.commons.utils.SprayCommonFormats.{offsetDateTimeFormat, uuidFormat}
import it.pagopa.interop.commons.utils.TypeConversions.TryOps
import it.pagopa.interop.commons.utils.errors.ComponentError
import it.pagopa.interop.purposeprocess.model._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.{ExecutionContext, Future}

package object impl extends SprayJsonSupport with DefaultJsonProtocol {

  type ManagementPurpose = purposemanagement.client.model.Purpose

  implicit def clientFormat: RootJsonFormat[Client]                 = jsonFormat2(Client)
  implicit def organizationFormat: RootJsonFormat[Organization]     = jsonFormat2(Organization)
  implicit def descriptorFormat: RootJsonFormat[EServiceDescriptor] = jsonFormat4(EServiceDescriptor)
  implicit def eServiceFormat: RootJsonFormat[EService]             = jsonFormat4(EService)
  implicit def agreementFormat: RootJsonFormat[Agreement]           = jsonFormat2(Agreement)

  implicit def riskAnalysisFormFormat: RootJsonFormat[RiskAnalysisForm]             = jsonFormat2(RiskAnalysisForm)
  implicit def purposeVersionDocumentFormat: RootJsonFormat[PurposeVersionDocument] =
    jsonFormat3(PurposeVersionDocument)
  implicit def purposeFormat: RootJsonFormat[Purpose]                               = jsonFormat13(Purpose)
  implicit def purposeVersionFormat: RootJsonFormat[PurposeVersion]                 = jsonFormat8(PurposeVersion)
  implicit def purposesFormat: RootJsonFormat[Purposes]                             = jsonFormat1(Purposes)
  implicit def purposeSeedFormat: RootJsonFormat[PurposeSeed]                       = jsonFormat5(PurposeSeed)
  implicit def purposeUpdateContentFormat: RootJsonFormat[PurposeUpdateContent]     = jsonFormat3(PurposeUpdateContent)
  implicit def purposeVersionSeedFormat: RootJsonFormat[PurposeVersionSeed]         = jsonFormat1(PurposeVersionSeed)
  implicit def problemErrorFormat: RootJsonFormat[ProblemError]                     = jsonFormat2(ProblemError)
  implicit def problemFormat: RootJsonFormat[Problem]                               = jsonFormat5(Problem)
  implicit def waitingForApprovalPurposeVersionUpdateFormat
    : RootJsonFormat[WaitingForApprovalPurposeVersionUpdateContent] = jsonFormat1(
    WaitingForApprovalPurposeVersionUpdateContent
  )
  implicit def draftPurposeVersionUpdateFormat: RootJsonFormat[DraftPurposeVersionUpdateContent] = jsonFormat1(
    DraftPurposeVersionUpdateContent
  )

  final val entityMarshallerProblem: ToEntityMarshaller[Problem] = sprayJsonMarshaller[Problem]

  final val serviceErrorCodePrefix: String = "012"
  final val defaultProblemType: String     = "about:blank"
  final val defaultErrorMessage: String    = "Unknown error"

  def problemOf(httpError: StatusCode, error: ComponentError): Problem =
    Problem(
      `type` = defaultProblemType,
      status = httpError.intValue,
      title = httpError.defaultMessage,
      errors = Seq(
        ProblemError(
          code = s"$serviceErrorCodePrefix-${error.code}",
          detail = Option(error.getMessage).getOrElse(defaultErrorMessage)
        )
      )
    )

  def problemOf(httpError: StatusCode, errors: List[ComponentError]): Problem =
    Problem(
      `type` = defaultProblemType,
      status = httpError.intValue,
      title = httpError.defaultMessage,
      errors = errors.map(error =>
        ProblemError(
          code = s"$serviceErrorCodePrefix-${error.code}",
          detail = Option(error.getMessage).getOrElse(defaultErrorMessage)
        )
      )
    )

  def validateBearer(contexts: Seq[(String, String)], jwt: JWTReader)(implicit ec: ExecutionContext): Future[String] =
    for {
      bearer <- getFutureBearer(contexts)
      _      <- jwt.getClaims(bearer).toFuture
    } yield bearer
}
