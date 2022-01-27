package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Purpose => DependencyPurpose}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.Purpose

object PurposeConverter {
  def dependencyToApi(purpose: DependencyPurpose): Purpose =
    Purpose(
      id = purpose.id,
      eserviceId = purpose.eserviceId,
      consumerId = purpose.consumerId,
      versions = purpose.versions.map(PurposeVersionConverter.dependencyToApi),
      suspendedByConsumer = purpose.suspendedByConsumer,
      suspendedByProducer = purpose.suspendedByProducer,
      title = purpose.title,
      description = purpose.description,
      createdAt = purpose.createdAt,
      updatedAt = purpose.updatedAt
    )
}
