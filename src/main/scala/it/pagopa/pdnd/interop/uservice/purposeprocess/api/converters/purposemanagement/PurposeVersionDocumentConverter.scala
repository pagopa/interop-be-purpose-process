package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  PurposeVersionDocument => DependencyPurposeVersionDocument
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposeVersionDocument

object PurposeVersionDocumentConverter {
  def dependencyToApi(document: DependencyPurposeVersionDocument): PurposeVersionDocument =
    PurposeVersionDocument(id = document.id, contentType = document.contentType, createdAt = document.createdAt)
}
