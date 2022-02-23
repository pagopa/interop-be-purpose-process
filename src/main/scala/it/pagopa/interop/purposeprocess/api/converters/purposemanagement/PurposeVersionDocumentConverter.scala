package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{PurposeVersionDocument => DependencyPurposeVersionDocument}
import it.pagopa.interop.purposeprocess.model.PurposeVersionDocument

object PurposeVersionDocumentConverter {
  def dependencyToApi(document: DependencyPurposeVersionDocument): PurposeVersionDocument =
    PurposeVersionDocument(id = document.id, contentType = document.contentType, createdAt = document.createdAt)
}
