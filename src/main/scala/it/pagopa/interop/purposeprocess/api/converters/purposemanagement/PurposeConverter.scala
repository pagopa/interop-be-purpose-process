package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{Purpose => DependencyPurpose}
import it.pagopa.interop.purposeprocess.model.{Agreement, Client, EService, Organization, OldPurpose}

object PurposeConverter {
  def dependencyToApi(
    purpose: DependencyPurpose,
    eService: EService,
    agreement: Agreement,
    consumer: Organization,
    clients: Seq[Client]
  ): OldPurpose = OldPurpose(
    id = purpose.id,
    agreement = agreement,
    eservice = eService,
    clients = clients,
    consumer = consumer,
    versions = purpose.versions.map(PurposeVersionConverter.dependencyToApi),
    suspendedByConsumer = purpose.suspendedByConsumer,
    suspendedByProducer = purpose.suspendedByProducer,
    title = purpose.title,
    riskAnalysisForm = purpose.riskAnalysisForm.map(RiskAnalysisConverter.dependencyToApi),
    description = purpose.description,
    createdAt = purpose.createdAt,
    updatedAt = purpose.updatedAt
  )
}
