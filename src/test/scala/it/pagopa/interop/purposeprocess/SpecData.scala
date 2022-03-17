package it.pagopa.interop.purposeprocess

import it.pagopa.interop.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.interop.partymanagement.client.{model => PartyManagement}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagement}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.interop.agreementmanagement.client.{model => AgreementManagement}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.model._

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

  val descriptor: CatalogManagement.EServiceDescriptor = CatalogManagement.EServiceDescriptor(
    id = UUID.randomUUID(),
    version = "1",
    description = None,
    audience = Seq.empty,
    voucherLifespan = 1,
    dailyCallsPerConsumer = 10000,
    dailyCallsTotal = 100000,
    interface = None,
    docs = Seq.empty,
    state = CatalogManagement.EServiceDescriptorState.PUBLISHED
  )

  val organization: PartyManagement.Organization = PartyManagement.Organization(
    id = UUID.randomUUID(),
    institutionId = UUID.randomUUID().toString,
    description = "Organization description",
    digitalAddress = "address",
    address = "address",
    taxCode = "taxCode",
    attributes = Seq.empty,
    zipCode = "00000"
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

  val validRiskAnalysis: RiskAnalysisForm = RiskAnalysisForm(
    version = "1.0",
    answers = RiskAnalysisFormAnswers(
      purpose = "purpose",
      usesPersonalData = RiskAnalysisFormYesNoAnswer.YES,
      usesThirdPartyPersonalData = None,
      usesConfidentialData = None,
      securedDataAccess = None,
      legalBasis = Some(Seq(FormLegalBasisAnswers.LEGAL_OBLIGATION, FormLegalBasisAnswers.PUBLIC_INTEREST)),
      legalObligationReference = Some("something"),
      publicInterestReference = Some("something"),
      knowsAccessedDataCategories = Some(RiskAnalysisFormYesNoAnswer.YES),
      accessDataArt9Gdpr = Some(RiskAnalysisFormYesNoAnswer.NO),
      accessUnderageData = Some(RiskAnalysisFormYesNoAnswer.NO),
      knowsDataQuantity = Some(RiskAnalysisFormYesNoAnswer.NO),
      dataQuantity = None,
      deliveryMethod = Some(FormDeliveryMethodAnswers.ANONYMOUS),
      doneDpia = Some(RiskAnalysisFormYesNoAnswer.NO),
      definedDataRetentionPeriod = Some(RiskAnalysisFormYesNoAnswer.NO),
      purposePursuit = Some(FormPurposePursuitAnswers.MERE_CORRECTNESS),
      checkedExistenceMereCorrectnessInteropCatalogue = Some(Seq(RiskAnalysisFormYesAnswer.YES)),
      checkedAllDataNeeded = None,
      checkedExistenceMinimalDataInteropCatalogue = None
    )
  )

  val validManagementRiskAnalysisSeed: PurposeManagement.RiskAnalysisFormSeed =
    RiskAnalysisValidation.validate(validRiskAnalysis).toOption.get

  val validManagementRiskAnalysis: PurposeManagement.RiskAnalysisForm =
    PurposeManagement.RiskAnalysisForm(
      id = UUID.randomUUID(),
      version = validManagementRiskAnalysisSeed.version,
      singleAnswers = validManagementRiskAnalysisSeed.singleAnswers.map(a =>
        PurposeManagement.RiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = a.key, value = a.value)
      ),
      multiAnswers = validManagementRiskAnalysisSeed.multiAnswers.map(a =>
        PurposeManagement.RiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = a.key, values = a.values)
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
    description = "A description",
    riskAnalysisForm = Some(validManagementRiskAnalysis),
    createdAt = timestamp,
    updatedAt = None
  )

  val purposeVersion: PurposeManagement.PurposeVersion = PurposeManagement.PurposeVersion(
    id = UUID.randomUUID(),
    state = PurposeManagement.PurposeVersionState.DRAFT,
    createdAt = timestamp,
    updatedAt = None,
    firstActivationAt = None,
    expectedApprovalDate = None,
    dailyCalls = 1000,
    riskAnalysis = None
  )

  def draftUpdate(dailyCalls: Int): DraftPurposeVersionUpdateContent = DraftPurposeVersionUpdateContent(dailyCalls)

  def waitingForApprovalUpdate: WaitingForApprovalPurposeVersionUpdateContent =
    WaitingForApprovalPurposeVersionUpdateContent(timestamp)

  val purposes: PurposeManagement.Purposes = PurposeManagement.Purposes(Seq(purpose))

  val agreement: AgreementManagement.Agreement = AgreementManagement.Agreement(
    id = UUID.randomUUID(),
    eserviceId = UUID.randomUUID(),
    descriptorId = UUID.randomUUID(),
    producerId = UUID.randomUUID(),
    consumerId = UUID.randomUUID(),
    state = AgreementManagement.AgreementState.ACTIVE,
    verifiedAttributes = Seq.empty,
    suspendedByConsumer = None,
    suspendedByProducer = None,
    createdAt = timestamp,
    updatedAt = None
  )

  val client: AuthorizationManagement.Client =
    AuthorizationManagement.Client(
      id = UUID.randomUUID(),
      consumerId = UUID.randomUUID(),
      name = "Client",
      description = None,
      purposes = Seq.empty,
      relationships = Set.empty,
      kind = AuthorizationManagement.ClientKind.CONSUMER
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
