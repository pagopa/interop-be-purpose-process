package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{Purpose => DependencyPurpose}
import it.pagopa.interop.purposemanagement.model.purpose.PersistentPurpose
import it.pagopa.interop.purposeprocess.model._

object PurposeConverter {

  def dependencyToApi(purpose: DependencyPurpose, eService: EService, consumer: Organization): Purpose = Purpose(
    id = purpose.id,
    eserviceId = eService.id,
    consumerId = consumer.id,
    versions = purpose.versions.map(PurposeVersionConverter.dependencyToApi),
    suspendedByConsumer = purpose.suspendedByConsumer,
    suspendedByProducer = purpose.suspendedByProducer,
    title = purpose.title,
    description = purpose.description,
    riskAnalysisForm = purpose.riskAnalysisForm.map(RiskAnalysisConverter.dependencyToApi),
    createdAt = purpose.createdAt,
    updatedAt = purpose.updatedAt
  )

  def persistentToApi(purpose: PersistentPurpose): Purpose = Purpose(
    id = purpose.id,
    eserviceId = purpose.eserviceId,
    consumerId = purpose.consumerId,
    versions = purpose.versions.map(PurposeVersionConverter.persistentToApi),
    suspendedByConsumer = purpose.suspendedByConsumer,
    suspendedByProducer = purpose.suspendedByProducer,
    title = purpose.title,
    description = purpose.description,
    riskAnalysisForm = purpose.riskAnalysisForm.map(RiskAnalysisConverter.persistentToApi),
    createdAt = purpose.createdAt,
    updatedAt = purpose.updatedAt
  )
}
