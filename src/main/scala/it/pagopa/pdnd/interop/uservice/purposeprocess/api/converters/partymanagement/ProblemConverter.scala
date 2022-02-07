package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.partymanagement

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.{
  Problem => PartyProblem,
  ProblemError => PartyProblemError
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, ProblemError}
import spray.json._

import scala.util.Try

object ProblemConverter extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def problemErrorFormat: RootJsonFormat[PartyProblemError] = jsonFormat2(PartyProblemError)
  implicit def problemFormat: RootJsonFormat[PartyProblem]           = jsonFormat5(PartyProblem)

  def dependencyToApi(problem: PartyProblem): Problem = Problem(
    `type` = problem.`type`,
    status = problem.status,
    title = problem.title,
    detail = problem.detail,
    errors = problem.errors.map(problemErrorToApi)
  )

  def fromString(body: String): Try[Problem] =
    Try(body.parseJson.convertTo[PartyProblem]).map(problem =>
      Problem(
        `type` = problem.`type`,
        status = problem.status,
        title = problem.title,
        detail = problem.detail,
        errors = problem.errors.map(problemErrorToApi)
      )
    )

  def problemErrorToApi(problemError: PartyProblemError): ProblemError =
    ProblemError(code = problemError.code, detail = problemError.detail)
}
