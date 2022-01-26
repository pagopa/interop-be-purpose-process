package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.catalogmanagement

import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.{
  Problem => CatalogProblem,
  ProblemError => CatalogProblemError
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, ProblemError}

object ProblemConverter {
  def dependencyToApi(problem: CatalogProblem): Problem = Problem(
    `type` = problem.`type`,
    status = problem.status,
    title = problem.title,
    detail = problem.detail,
    errors = problem.errors.map(problemErrorToApi)
  )

  def problemErrorToApi(problemError: CatalogProblemError): ProblemError =
    ProblemError(code = problemError.code, detail = problemError.detail)
}
