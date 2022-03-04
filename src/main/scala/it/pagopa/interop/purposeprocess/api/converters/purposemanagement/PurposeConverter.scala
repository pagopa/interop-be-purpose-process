package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.{Purpose => DependencyPurpose}
import it.pagopa.interop.purposeprocess.model.{Agreement, Client, EService, Purpose}

object PurposeConverter {
  def dependencyToApi(
    purpose: DependencyPurpose,
    eService: EService,
    agreement: Agreement,
    clients: Seq[Client]
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
