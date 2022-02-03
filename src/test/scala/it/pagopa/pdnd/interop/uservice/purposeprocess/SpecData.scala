package it.pagopa.pdnd.interop.uservice.purposeprocess

import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.{model => PartyManagement}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{
  FormAccessDataArt9GdprAnswers,
  FormAccessUnderageDataAnswers,
  FormCheckedExistenceMereCorrectnessPdndCatalogueAnswers,
  FormDefinedDataRetentionPeriodAnswers,
  FormDeliveryMethodAnswers,
  FormDoneDpiaAnswers,
  FormKnowsAccessedDataCategoriesAnswers,
  FormKnowsDataQuantityAnswers,
  FormLegalBasisAnswers,
  FormPurposePursuitAnswers,
  FormUsesPersonalDataAnswers,
  RiskAnalysisForm,
  RiskAnalysisFormAnswers
}

import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID

object SpecData {
  final val timestamp = OffsetDateTime.of(2022, 12, 31, 11, 22, 33, 44, ZoneOffset.UTC)

  val eService: CatalogManagement.EService = CatalogManagement.EService(
    id = UUID.randomUUID(),
    producerId = UUID.randomUUID(),
    name = "EService Name",
    description = "EService Description",
    technology = CatalogManagement.EServiceTechnology.REST,
    attributes = CatalogManagement.Attributes(Seq.empty, Seq.empty, Seq.empty),
    descriptors = Seq.empty
  )

  val organization: PartyManagement.Organization = PartyManagement.Organization(
    id = UUID.randomUUID(),
    institutionId = UUID.randomUUID().toString,
    description = "Organization description",
    digitalAddress = "address",
    taxCode = "taxCode",
    attributes = Seq.empty
  )

  def relationships(from: UUID = UUID.randomUUID(), to: UUID = UUID.randomUUID()): PartyManagement.Relationships =
    PartyManagement.Relationships(items =
      Seq(
        PartyManagement.Relationship(
          id = UUID.randomUUID(),
          from = from,
          to = to,
          filePath = None,
          fileName = None,
          contentType = None,
          tokenId = None,
          role = PartyManagement.PartyRole.MANAGER,
          product = PartyManagement.RelationshipProduct("a", "b", timestamp),
          state = PartyManagement.RelationshipState.ACTIVE,
          createdAt = timestamp,
          updatedAt = None
        )
      )
    )

  val purpose: PurposeManagement.Purpose = PurposeManagement.Purpose(
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

  val purposes: PurposeManagement.Purposes = PurposeManagement.Purposes(Seq(purpose))

  val validRiskAnalysis: RiskAnalysisForm = RiskAnalysisForm(
    version = "1.0",
    answers = RiskAnalysisFormAnswers(
      purpose = "purpose",
      usesPersonalData = FormUsesPersonalDataAnswers.usesPersonalDataYes,
      usesThirdPartyPersonalData = None,
      usesConfidentialData = None,
      securedDataAccess = None,
      legalBasis =
        Some(Seq(FormLegalBasisAnswers.legalBasisLegalObligation, FormLegalBasisAnswers.legalBasisPublicInterest)),
      legalObligationReference = Some("something"),
      publicInterestReference = Some("something"),
      knowsAccessedDataCategories = Some(FormKnowsAccessedDataCategoriesAnswers.knowsAccessedDataCategoriesYes),
      accessDataArt9Gdpr = Some(FormAccessDataArt9GdprAnswers.accessDataArt9GdprNo),
      accessUnderageData = Some(FormAccessUnderageDataAnswers.accessUnderageDataNo),
      knowsDataQuantity = Some(FormKnowsDataQuantityAnswers.knowsDataQuantityNo),
      dataQuantity = None,
      deliveryMethod = Some(FormDeliveryMethodAnswers.deliveryMethodAnonymous),
      doneDpia = Some(FormDoneDpiaAnswers.doneDpiaNo),
      definedDataRetentionPeriod = Some(FormDefinedDataRetentionPeriodAnswers.definedDataRetentionPeriodNo),
      purposePursuit = Some(FormPurposePursuitAnswers.purposePursuitMereCorrectness),
      checkedExistenceMereCorrectnessPdndCatalogue =
        Some(Seq(FormCheckedExistenceMereCorrectnessPdndCatalogueAnswers.checkedExistencePdndCatalogueYes)),
      checkedAllDataNeeded = None,
      checkedExistenceMinimalDataPdndCatalogue = None
    )
  )

  val catalogProblem: CatalogManagement.Problem = CatalogManagement.Problem(
    `type` = "something",
    status = 400,
    title = "A title",
    detail = None,
    errors = Seq(CatalogManagement.ProblemError(code = "AAA-BBBB", detail = "Error details"))
  )

  val partyProblem: PartyManagement.Problem = PartyManagement.Problem(
    `type` = "something",
    status = 400,
    title = "A title",
    detail = None,
    errors = Seq(PartyManagement.ProblemError(code = "AAA-BBBB", detail = "Error details"))
  )

  val purposeProblem: PurposeManagement.Problem = PurposeManagement.Problem(
    `type` = "something",
    status = 400,
    title = "A title",
    detail = None,
    errors = Seq(PurposeManagement.ProblemError(code = "AAA-BBBB", detail = "Error details"))
  )

}
