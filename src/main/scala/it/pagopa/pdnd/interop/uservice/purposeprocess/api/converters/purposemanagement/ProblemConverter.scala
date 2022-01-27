package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  Problem => PurposeProblem,
  ProblemError => PurposeProblemError
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, ProblemError}

object ProblemConverter {
  def dependencyToApi(problem: PurposeProblem): Problem = Problem(
    `type` = problem.`type`,
    status = problem.status,
    title = problem.title,
    detail = problem.detail,
    errors = problem.errors.map(problemErrorToApi)
  )

  def problemErrorToApi(problemError: PurposeProblemError): ProblemError =
    ProblemError(code = problemError.code, detail = problemError.detail)
}
