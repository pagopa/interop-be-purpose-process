package it.pagopa.interop.purposeprocess

import cats.syntax.all._
import it.pagopa.interop.agreementmanagement.client.model.Stamps
import it.pagopa.interop.agreementmanagement.client.{model => AgreementManagement}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagement}
import it.pagopa.interop.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.interop.commons.utils.service.OffsetDateTimeSupplier
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.interop.purposemanagement.model.purpose.{
  PersistentPurpose,
  PersistentRiskAnalysisForm,
  PersistentRiskAnalysisMultiAnswer,
  PersistentRiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.tenantmanagement.client.model.{ExternalId, Tenant, TenantKind}

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
    state = CatalogManagement.EServiceDescriptorState.PUBLISHED,
    agreementApprovalPolicy = CatalogManagement.AgreementApprovalPolicy.AUTOMATIC,
    serverUrls = List.empty
  )

  val tenant: Tenant = Tenant(
    UUID.randomUUID(),
    TenantKind.PA,
    selfcareId = UUID.randomUUID.toString.some,
    externalId = ExternalId("foo", "bar"),
    features = Nil,
    attributes = Nil,
    createdAt = OffsetDateTimeSupplier.get(),
    updatedAt = None,
    mails = Nil,
    name = "test_name"
  )

  val validRiskAnalysis1_0: RiskAnalysisForm = RiskAnalysisForm(
    version = "1.0",
    answers = Map(
      "purpose"                                         -> List("MyPurpose"),
      "usesPersonalData"                                -> List("YES"),
      "usesThirdPartyPersonalData"                      -> Nil,
      "usesConfidentialData"                            -> Nil,
      "securedDataAccess"                               -> Nil,
      "legalBasis"                                      -> List("LEGAL_OBLIGATION", "PUBLIC_INTEREST"),
      "legalObligationReference"                        -> List("somethingLegal"),
      "publicInterestReference"                         -> List("somethingPublic"),
      "knowsAccessedDataCategories"                     -> List("YES"),
      "accessDataArt9Gdpr"                              -> List("NO"),
      "accessUnderageData"                              -> List("NO"),
      "knowsDataQuantity"                               -> List("NO"),
      "dataQuantity"                                    -> Nil,
      "deliveryMethod"                                  -> List("ANONYMOUS"),
      "doneDpia"                                        -> List("NO"),
      "definedDataRetentionPeriod"                      -> List("NO"),
      "purposePursuit"                                  -> List("MERE_CORRECTNESS"),
      "checkedExistenceMereCorrectnessInteropCatalogue" -> List("YES"),
      "checkedAllDataNeeded"                            -> Nil,
      "checkedExistenceMinimalDataInteropCatalogue"     -> Nil
    )
  )

  val validRiskAnalysis2_0: RiskAnalysisForm = RiskAnalysisForm(
    version = "2.0",
    answers = Map(
      "purpose"                                         -> List("INSTITUTIONAL"),
      "institutionalPurpose"                            -> List("MyPurpose"),
      "personalDataTypes"                               -> List("OTHER"),
      "otherPersonalDataTypes"                          -> List("MyDataTypes"),
      "legalBasis"                                      -> List("LEGAL_OBLIGATION", "PUBLIC_INTEREST"),
      "legalObligationReference"                        -> List("somethingLegal"),
      "legalBasisPublicInterest"                        -> List("RULE_OF_LAW"),
      "ruleOfLawText"                                   -> List("TheLaw"),
      "knowsDataQuantity"                               -> List("NO"),
      "dataQuantity"                                    -> Nil,
      "deliveryMethod"                                  -> List("ANONYMOUS"),
      "policyProvided"                                  -> List("NO"),
      "confirmPricipleIntegrityAndDiscretion"           -> List("true"),
      "reasonPolicyNotProvided"                         -> List("Because"),
      "doneDpia"                                        -> List("NO"),
      "dataRetentionPeriod"                             -> List("true"),
      "purposePursuit"                                  -> List("MERE_CORRECTNESS"),
      "checkedExistenceMereCorrectnessInteropCatalogue" -> List("true"),
      "usesThirdPartyData"                              -> List("NO"),
      "declarationConfirmGDPR"                          -> List("true")
    )
  )

  val validManagementRiskAnalysisSeed: PurposeManagement.RiskAnalysisFormSeed =
    RiskAnalysisValidation.validate(validRiskAnalysis1_0)(tenant.kind).toOption.get

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

  val validPersistentRiskAnalysis: PersistentRiskAnalysisForm =
    PersistentRiskAnalysisForm(
      id = UUID.randomUUID(),
      version = validManagementRiskAnalysisSeed.version,
      singleAnswers = validManagementRiskAnalysisSeed.singleAnswers.map(a =>
        PersistentRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = a.key, value = a.value)
      ),
      multiAnswers = validManagementRiskAnalysisSeed.multiAnswers.map(a =>
        PersistentRiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = a.key, values = a.values)
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

  val persistentPurpose: PersistentPurpose = PersistentPurpose(
    id = UUID.randomUUID(),
    eserviceId = UUID.randomUUID(),
    consumerId = UUID.randomUUID(),
    versions = Seq.empty,
    suspendedByConsumer = None,
    suspendedByProducer = None,
    title = "A title",
    description = "A description",
    riskAnalysisForm = Some(validPersistentRiskAnalysis),
    createdAt = timestamp,
    updatedAt = None
  )

  def draftUpdate(dailyCalls: Int): DraftPurposeVersionUpdateContent = DraftPurposeVersionUpdateContent(dailyCalls)

  def waitingForApprovalUpdate: WaitingForApprovalPurposeVersionUpdateContent =
    WaitingForApprovalPurposeVersionUpdateContent(timestamp)

  val purposes: PurposeManagement.Purposes = PurposeManagement.Purposes(Seq(purpose))

  val agreement: AgreementManagement.Agreement =
    AgreementManagement.Agreement(
      id = UUID.randomUUID(),
      eserviceId = UUID.randomUUID(),
      descriptorId = UUID.randomUUID(),
      producerId = UUID.randomUUID(),
      consumerId = UUID.randomUUID(),
      state = AgreementManagement.AgreementState.ACTIVE,
      verifiedAttributes = List.empty,
      certifiedAttributes = List.empty,
      declaredAttributes = List.empty,
      suspendedByConsumer = None,
      suspendedByProducer = None,
      suspendedByPlatform = None,
      consumerDocuments = List.empty,
      createdAt = timestamp,
      updatedAt = None,
      consumerNotes = None,
      stamps = Stamps()
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

}
