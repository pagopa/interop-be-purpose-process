package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.partymanagement

import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.{
  Problem => PartyProblem,
  ProblemError => PartyProblemError
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, ProblemError}

object ProblemConverter {
  def dependencyToApi(problem: PartyProblem): Problem = Problem(
    `type` = problem.`type`,
    status = problem.status,
    title = problem.title,
    detail = problem.detail,
    errors = problem.errors.map(problemErrorToApi)
  )

  def problemErrorToApi(problemError: PartyProblemError): ProblemError =
    ProblemError(code = problemError.code, detail = problemError.detail)
}
