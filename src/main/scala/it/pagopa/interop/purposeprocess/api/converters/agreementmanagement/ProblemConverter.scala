package it.pagopa.interop.purposeprocess.api.converters.agreementmanagement

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import it.pagopa.interop.agreementmanagement.client.model.{
  Problem => AgreementProblem,
  ProblemError => AgreementProblemError
}
import it.pagopa.interop.purposeprocess.model.{Problem, ProblemError}
import spray.json._

import scala.util.Try

object ProblemConverter extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def problemErrorFormat: RootJsonFormat[AgreementProblemError] = jsonFormat2(AgreementProblemError)
  implicit def problemFormat: RootJsonFormat[AgreementProblem]           = jsonFormat5(AgreementProblem)

  def dependencyToApi(problem: AgreementProblem): Problem = Problem(
    `type` = problem.`type`,
    status = problem.status,
    title = problem.title,
    detail = problem.detail,
    errors = problem.errors.map(problemErrorToApi)
  )

  def fromString(body: String): Try[Problem] =
    Try(body.parseJson.convertTo[AgreementProblem]).map(problem =>
      Problem(
        `type` = problem.`type`,
        status = problem.status,
        title = problem.title,
        detail = problem.detail,
        errors = problem.errors.map(problemErrorToApi)
      )
    )

  def problemErrorToApi(problemError: AgreementProblemError): ProblemError =
    ProblemError(code = problemError.code, detail = problemError.detail)
}
