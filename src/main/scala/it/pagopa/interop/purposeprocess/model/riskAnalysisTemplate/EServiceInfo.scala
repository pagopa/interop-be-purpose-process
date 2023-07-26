package it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate

final case class EServiceInfo(
  name: String,
  producerName: String,
  producerOrigin: String,
  producerIPACode: String,
  consumerName: String,
  consumerOrigin: String,
  consumerIPACode: String
)
