package it.pagopa.interop.purposeprocess.model

final case class EServiceInfo(
  name: String,
  producerName: String,
  producerOrigin: String,
  producerIPACode: String,
  consumerName: String,
  consumerOrigin: String,
  consumerIPACode: String
)
