package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import cats.implicits._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Purpose => DependencyPurpose}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Agreement, Clients, EService, Purpose}

object PurposeConverter {
  def dependencyToApi(
    purpose: DependencyPurpose,
    eService: EService,
    agreement: Agreement,
    clients: Clients
  ): Either[Throwable, Purpose] = {
    for {
      riskAnalysisForm <- purpose.riskAnalysisForm.traverse(RiskAnalysisConverter.dependencyToApi)
    } yield Purpose(
      id = purpose.id,
      agreement = agreement,
      eservice = eService,
      clients = clients,
      consumerId = purpose.consumerId,
      versions = purpose.versions.map(PurposeVersionConverter.dependencyToApi),
      suspendedByConsumer = purpose.suspendedByConsumer,
      suspendedByProducer = purpose.suspendedByProducer,
      title = purpose.title,
      riskAnalysisForm = riskAnalysisForm,
      description = purpose.description,
      createdAt = purpose.createdAt,
      updatedAt = purpose.updatedAt
    )
  }
}
