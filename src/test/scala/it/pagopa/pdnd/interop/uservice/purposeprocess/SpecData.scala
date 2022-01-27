package it.pagopa.pdnd.interop.uservice.purposeprocess

import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.{
  Attributes,
  EService,
  EServiceTechnology,
  Problem => CatalogProblem,
  ProblemError => CatalogProblemError
}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.{
  Organization,
  Problem => PartyProblem,
  ProblemError => PartyProblemError
}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  Purpose,
  Purposes,
  Problem => PurposeProblem,
  ProblemError => PurposeProblemError
}

import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID

object SpecData {
  final val timestamp = OffsetDateTime.of(2022, 12, 31, 11, 22, 33, 44, ZoneOffset.UTC)

  val eService: EService = EService(
    id = UUID.randomUUID(),
    producerId = UUID.randomUUID(),
    name = "EService Name",
    description = "EService Description",
    technology = EServiceTechnology.REST,
    attributes = Attributes(Seq.empty, Seq.empty, Seq.empty),
    descriptors = Seq.empty
  )

  val organization: Organization = Organization(
    id = UUID.randomUUID(),
    institutionId = UUID.randomUUID().toString,
    description = "Organization description",
    digitalAddress = "address",
    taxCode = "taxCode",
    attributes = Seq.empty
  )

  val purpose: Purpose = Purpose(
    id = UUID.randomUUID(),
    eserviceId = UUID.randomUUID(),
    consumerId = UUID.randomUUID(),
    versions = Seq.empty,
    suspendedByConsumer = None,
    suspendedByProducer = None,
    title = "A title",
    description = Some("A description"),
    createdAt = timestamp,
    updatedAt = None
  )

  val purposes: Purposes = Purposes(Seq(purpose))

  val catalogProblem: CatalogProblem = CatalogProblem(
    `type` = "something",
    status = 400,
    title = "A title",
    detail = None,
    errors = Seq(CatalogProblemError(code = "AAA-BBBB", detail = "Error details"))
  )

  val partyProblem: PartyProblem = PartyProblem(
    `type` = "something",
    status = 400,
    title = "A title",
    detail = None,
    errors = Seq(PartyProblemError(code = "AAA-BBBB", detail = "Error details"))
  )

  val purposeProblem: PurposeProblem = PurposeProblem(
    `type` = "something",
    status = 400,
    title = "A title",
    detail = None,
    errors = Seq(PurposeProblemError(code = "AAA-BBBB", detail = "Error details"))
  )

}
