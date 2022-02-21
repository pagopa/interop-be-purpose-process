package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.authorizationmanagement

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import it.pagopa.interop.authorizationmanagement.client.model.{
  Problem => AuthorizationProblem,
  ProblemError => AuthorizationProblemError
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, ProblemError}
import spray.json._

import scala.util.Try

object ProblemConverter extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def problemErrorFormat: RootJsonFormat[AuthorizationProblemError] = jsonFormat2(AuthorizationProblemError)
  implicit def problemFormat: RootJsonFormat[AuthorizationProblem]           = jsonFormat5(AuthorizationProblem)

  def dependencyToApi(problem: AuthorizationProblem): Problem = Problem(
    `type` = problem.`type`,
    status = problem.status,
    title = problem.title,
    detail = problem.detail,
    errors = problem.errors.map(problemErrorToApi)
  )

  def fromString(body: String): Try[Problem] =
    Try(body.parseJson.convertTo[AuthorizationProblem]).map(problem =>
      Problem(
        `type` = problem.`type`,
        status = problem.status,
        title = problem.title,
        detail = problem.detail,
        errors = problem.errors.map(problemErrorToApi)
      )
    )

  def problemErrorToApi(problemError: AuthorizationProblemError): ProblemError =
    ProblemError(code = problemError.code, detail = problemError.detail)
}
