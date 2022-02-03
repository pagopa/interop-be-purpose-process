package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import cats.implicits._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Purposes => DependencyPurposes}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.Purposes

object PurposesConverter {
  def dependencyToApi(purposes: DependencyPurposes): Either[Throwable, Purposes] = {
    for {
      purposes <- purposes.purposes.traverse(PurposeConverter.dependencyToApi)
    } yield Purposes(purposes = purposes)
  }
}
