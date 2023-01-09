package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{Purpose => DependencyPurpose}
import it.pagopa.interop.purposeprocess.model.{Agreement, Client, EService, OldPurpose, Organization, Purpose}

object PurposeConverter {
  def dependencyToOldApi(
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

  def dependencyToApi(purpose: DependencyPurpose): Purpose = Purpose(
    id = purpose.id,
    eserviceId = purpose.eserviceId,
    consumerId = purpose.consumerId,
    versions = purpose.versions.map(PurposeVersionConverter.dependencyToApi),
    suspendedByConsumer = purpose.suspendedByConsumer,
    suspendedByProducer = purpose.suspendedByProducer,
    title = purpose.title,
    description = purpose.description,
    riskAnalysisForm = purpose.riskAnalysisForm.map(RiskAnalysisConverter.dependencyToApi),
    createdAt = purpose.createdAt,
    updatedAt = purpose.updatedAt
  )
}
