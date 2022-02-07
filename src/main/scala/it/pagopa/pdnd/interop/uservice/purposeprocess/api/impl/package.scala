package it.pagopa.pdnd.interop.uservice.purposeprocess.api

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCode
import it.pagopa.pdnd.interop.commons.jwt.service.JWTReader
import it.pagopa.pdnd.interop.commons.utils.AkkaUtils.getFutureBearer
import it.pagopa.pdnd.interop.commons.utils.SprayCommonFormats.{offsetDateTimeFormat, uuidFormat}
import it.pagopa.pdnd.interop.commons.utils.TypeConversions.TryOps
import it.pagopa.pdnd.interop.commons.utils.errors.ComponentError
import it.pagopa.pdnd.interop.uservice._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.{ExecutionContext, Future}

package object impl extends SprayJsonSupport with DefaultJsonProtocol {

  type ManagementPurpose = purposemanagement.client.model.Purpose

  final val serviceErrorCodePrefix: String = "012"
  final val defaultProblemType: String     = "about:blank"

  implicit def riskAnalysisFormAnswersFormat: RootJsonFormat[RiskAnalysisFormAnswers] =
    jsonFormat20(RiskAnalysisFormAnswers)
  implicit def riskAnalysisFormFormat: RootJsonFormat[RiskAnalysisForm] = jsonFormat2(RiskAnalysisForm)
  implicit def purposeVersionDocumentFormat: RootJsonFormat[PurposeVersionDocument] =
    jsonFormat3(PurposeVersionDocument)
  implicit def purposeVersionFormat: RootJsonFormat[PurposeVersion] = jsonFormat6(PurposeVersion)
  implicit def purposeFormat: RootJsonFormat[Purpose]               = jsonFormat11(Purpose)
  implicit def purposesFormat: RootJsonFormat[Purposes]             = jsonFormat1(Purposes)
  implicit def purposeSeedFormat: RootJsonFormat[PurposeSeed]       = jsonFormat5(PurposeSeed)

  implicit def problemErrorFormat: RootJsonFormat[ProblemError] = jsonFormat2(ProblemError)
  implicit def problemFormat: RootJsonFormat[Problem]           = jsonFormat5(Problem)

  def problemOf(httpError: StatusCode, error: ComponentError, defaultMessage: String = "Unknown error"): Problem =
    Problem(
      `type` = defaultProblemType,
      status = httpError.intValue,
      title = httpError.defaultMessage,
      errors = Seq(
        ProblemError(
          code = s"$serviceErrorCodePrefix-${error.code}",
          detail = Option(error.getMessage).getOrElse(defaultMessage)
        )
      )
    )

  def validateBearer(contexts: Seq[(String, String)], jwt: JWTReader)(implicit ec: ExecutionContext): Future[String] =
    for {
      bearer <- getFutureBearer(contexts)
      _      <- jwt.getClaims(bearer).toFuture
    } yield bearer
}
