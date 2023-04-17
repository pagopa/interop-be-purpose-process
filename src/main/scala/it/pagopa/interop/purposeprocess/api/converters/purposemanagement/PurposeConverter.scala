package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{Purpose => DependencyPurpose}
import it.pagopa.interop.purposemanagement.model.purpose.PersistentPurpose
import it.pagopa.interop.purposeprocess.model._

object PurposeConverter {

  implicit class DependencyPurposeWrapper(private val purpose: DependencyPurpose) extends AnyVal {
    def dependencyToApi(isRiskAnalysisValid: Boolean): Purpose = Purpose(
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
      updatedAt = purpose.updatedAt,
      isRiskAnalysisValid = isRiskAnalysisValid
    )
  }

  implicit class PersistentPurposeWrapper(private val purpose: PersistentPurpose) extends AnyVal {
    def persistentToApi: Purpose = Purpose(
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
      updatedAt = purpose.updatedAt,
      isRiskAnalysisValid = false //Ugly fake value here, but I see the point in removing a not required activity that may impact performances in listing.

    )
  }
}
