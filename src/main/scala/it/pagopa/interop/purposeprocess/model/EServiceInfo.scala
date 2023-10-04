package it.pagopa.interop.purposeprocess.model

import it.pagopa.interop.catalogmanagement.model.CatalogItemMode

final case class EServiceInfo(
  name: String,
  mode: CatalogItemMode,
  producerName: String,
  producerOrigin: String,
  producerIPACode: String,
  consumerName: String,
  consumerOrigin: String,
  consumerIPACode: String
)
