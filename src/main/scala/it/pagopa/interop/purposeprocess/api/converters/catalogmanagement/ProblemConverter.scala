package it.pagopa.interop.purposeprocess.api.converters.catalogmanagement

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import it.pagopa.interop.catalogmanagement.client.model.{Problem => CatalogProblem, ProblemError => CatalogProblemError}
import it.pagopa.interop.purposeprocess.model.{Problem, ProblemError}
import spray.json._

import scala.util.Try

object ProblemConverter extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def problemErrorFormat: RootJsonFormat[CatalogProblemError] = jsonFormat2(CatalogProblemError)
  implicit def problemFormat: RootJsonFormat[CatalogProblem]           = jsonFormat5(CatalogProblem)

  def dependencyToApi(problem: CatalogProblem): Problem = Problem(
    `type` = problem.`type`,
    status = problem.status,
    title = problem.title,
    detail = problem.detail,
    errors = problem.errors.map(problemErrorToApi)
  )

  def fromString(body: String): Try[Problem] =
    Try(body.parseJson.convertTo[CatalogProblem]).map(problem =>
      Problem(
        `type` = problem.`type`,
        status = problem.status,
        title = problem.title,
        detail = problem.detail,
        errors = problem.errors.map(problemErrorToApi)
      )
    )

  def problemErrorToApi(problemError: CatalogProblemError): ProblemError =
    ProblemError(code = problemError.code, detail = problemError.detail)
}
