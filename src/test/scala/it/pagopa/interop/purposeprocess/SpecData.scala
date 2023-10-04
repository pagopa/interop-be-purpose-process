package it.pagopa.interop.purposeprocess

import cats.syntax.all._
import it.pagopa.interop.commons.utils.service.OffsetDateTimeSupplier
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.interop.purposemanagement.model.purpose.{
  PersistentPurpose,
  PersistentRiskAnalysisForm,
  PersistentRiskAnalysisMultiAnswer,
  PersistentRiskAnalysisSingleAnswer,
  Draft,
  PersistentPurposeVersion,
  Active => PurposeActive
}
import it.pagopa.interop.commons.riskanalysis.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.catalogmanagement.model.{
  CatalogItem,
  CatalogAttributes,
  Rest,
  CatalogDescriptor,
  Published,
  Automatic,
  Deliver,
  CatalogRiskAnalysis,
  CatalogRiskAnalysisForm,
  CatalogRiskAnalysisSingleAnswer,
  CatalogRiskAnalysisMultiAnswer
}
import it.pagopa.interop.authorizationmanagement.model.client.{PersistentClient, Consumer}
import it.pagopa.interop.agreementmanagement.model.agreement.{Active, PersistentStamps, PersistentAgreement}
import it.pagopa.interop.tenantmanagement.model.tenant.{PersistentTenantKind, PersistentTenant, PersistentExternalId}
import it.pagopa.interop.purposeprocess.api.Adapters._
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID

object SpecData {

  final val timestamp = OffsetDateTime.of(2022, 12, 31, 11, 22, 33, 44, ZoneOffset.UTC)

  val riskAnalysisOnlySchemaSeed: PurposeManagement.RiskAnalysisFormSeed = PurposeManagement.RiskAnalysisFormSeed(
    riskAnalysisId = Some(UUID.randomUUID()),
    version = "3.0",
    singleAnswers = Seq(PurposeManagement.RiskAnalysisSingleAnswerSeed(key = "purpose", value = Some("INSTITUTIONAL"))),
    multiAnswers = Seq(PurposeManagement.RiskAnalysisMultiAnswerSeed(key = "personalDataTypes", values = Seq("OTHER")))
  )

  val riskAnalysisOnlySchema: CatalogRiskAnalysis = CatalogRiskAnalysis(
    id = UUID.randomUUID(),
    name = "EService Risk Analysis",
    riskAnalysisForm = CatalogRiskAnalysisForm(
      id = UUID.randomUUID(),
      version = "3.0",
      singleAnswers =
        Seq(CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "purpose", value = Some("INSTITUTIONAL"))),
      multiAnswers =
        Seq(CatalogRiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = "personalDataTypes", values = Seq("OTHER")))
    ),
    createdAt = OffsetDateTimeSupplier.get()
  )

  val riskAnalysis: CatalogRiskAnalysis = CatalogRiskAnalysis(
    id = UUID.randomUUID(),
    name = "EService Risk Analysis",
    riskAnalysisForm = CatalogRiskAnalysisForm(
      id = UUID.randomUUID(),
      version = "3.0",
      singleAnswers = Seq(
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "purpose", value = Some("INSTITUTIONAL")),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "legalObligationReference", value = Some("YES")),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "dataDownload", value = Some("YES")),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "checkedExistenceMereCorrectnessInteropCatalogue",
          value = Some("true")
        ),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "deliveryMethod", value = Some("CLEARTEXT")),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "legalBasisPublicInterest",
          value = Some("RULE_OF_LAW")
        ),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "confirmPricipleIntegrityAndDiscretion",
          value = Some("true")
        ),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "ruleOfLawText", value = Some("TheLaw")),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "confirmDataRetentionPeriod",
          value = Some("true")
        ),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "usesThirdPartyData", value = Some("YES")),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "otherPersonalDataTypes",
          value = Some("MyThirdPartyData")
        ),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "doesUseThirdPartyData", value = Some("YES")),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "knowsDataQuantity", value = Some("NO")),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "institutionalPurpose",
          value = Some("MyPurpose")
        ),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "policyProvided", value = Some("NO")),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "reasonPolicyNotProvided",
          value = Some("Because")
        ),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "doneDpia", value = Some("NO")),
        CatalogRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = "declarationConfirmGDPR", value = Some("true")),
        CatalogRiskAnalysisSingleAnswer(
          id = UUID.randomUUID(),
          key = "purposePursuit",
          value = Some("MERE_CORRECTNESS")
        )
      ),
      multiAnswers = Seq(
        CatalogRiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = "personalDataTypes", values = Seq("OTHER")),
        CatalogRiskAnalysisMultiAnswer(
          id = UUID.randomUUID(),
          key = "legalBasis",
          values = Seq("LEGAL_OBLIGATION", "PUBLIC_INTEREST")
        )
      )
    ),
    createdAt = OffsetDateTimeSupplier.get()
  )

  val eService: CatalogItem = CatalogItem(
    id = UUID.randomUUID(),
    producerId = UUID.randomUUID(),
    name = "EService Name",
    description = "EService Description",
    technology = Rest,
    attributes = CatalogAttributes.empty.some,
    descriptors = Seq.empty,
    createdAt = OffsetDateTimeSupplier.get(),
    riskAnalysis = Seq.empty,
    mode = Deliver
  )

  val descriptor: CatalogDescriptor = CatalogDescriptor(
    id = UUID.randomUUID(),
    version = "1",
    description = None,
    audience = Seq.empty,
    voucherLifespan = 1,
    dailyCallsPerConsumer = 10000,
    dailyCallsTotal = 100000,
    interface = None,
    docs = Seq.empty,
    state = Published,
    agreementApprovalPolicy = Some(Automatic),
    serverUrls = List.empty,
    createdAt = OffsetDateTimeSupplier.get(),
    publishedAt = Some(OffsetDateTimeSupplier.get()),
    suspendedAt = None,
    deprecatedAt = None,
    archivedAt = None,
    attributes = CatalogAttributes.empty
  )

  val tenant: PersistentTenant = PersistentTenant(
    id = UUID.randomUUID(),
    kind = PersistentTenantKind.PA.some,
    selfcareId = UUID.randomUUID.toString.some,
    externalId = PersistentExternalId("foo", "bar"),
    features = Nil,
    attributes = Nil,
    createdAt = OffsetDateTimeSupplier.get(),
    updatedAt = None,
    mails = Nil,
    name = "test_name"
  )

  val validRiskAnalysis3_0_Pa: RiskAnalysisForm = RiskAnalysisForm(
    version = "3.0",
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
      "dataDownload"                                    -> List("YES"),
      "confirmDataRetentionPeriod"                      -> List("true"),
      "purposePursuit"                                  -> List("MERE_CORRECTNESS"),
      "checkedExistenceMereCorrectnessInteropCatalogue" -> List("true"),
      "usesThirdPartyData"                              -> List("NO"),
      "declarationConfirmGDPR"                          -> List("true")
    )
  )

  val validRiskAnalysis2_0_Private: RiskAnalysisForm = RiskAnalysisForm(
    version = "2.0",
    answers = Map(
      "purpose"                                         -> List("INSTITUTIONAL"),
      "institutionalPurpose"                            -> List("MyPurpose"),
      "usesPersonalData"                                -> List("YES"),
      "personalDataTypes"                               -> List("OTHER"),
      "otherPersonalDataTypes"                          -> List("MyDataTypes"),
      "legalBasis"                                      -> List("LEGAL_OBLIGATION", "PUBLIC_INTEREST"),
      "legalObligationReference"                        -> List("YES"),
      "legalBasisPublicInterest"                        -> List("RULE_OF_LAW"),
      "ruleOfLawText"                                   -> List("TheLaw"),
      "knowsDataQuantity"                               -> List("NO"),
      "dataQuantity"                                    -> Nil,
      "dataDownload"                                    -> List("YES"),
      "deliveryMethod"                                  -> List("CLEARTEXT"),
      "policyProvided"                                  -> List("NO"),
      "confirmPricipleIntegrityAndDiscretion"           -> List("true"),
      "reasonPolicyNotProvided"                         -> List("Because"),
      "doneDpia"                                        -> List("NO"),
      "dataRetentionPeriod"                             -> List("10"),
      "purposePursuit"                                  -> List("MERE_CORRECTNESS"),
      "checkedExistenceMereCorrectnessInteropCatalogue" -> List("true"),
      "declarationConfirmGDPR"                          -> List("true")
    )
  )

  val validOnlySchemaRiskAnalysis2_0: RiskAnalysisForm = RiskAnalysisForm(
    version = "3.0",
    answers = Map(
      "purpose"                    -> List("INSTITUTIONAL"),
      "usesPersonalData"           -> Nil,
      "usesThirdPartyPersonalData" -> Nil,
      "usesConfidentialData"       -> Nil
    )
  )

  val validOnlySchemaRiskAnalysis1_0: RiskAnalysisForm = RiskAnalysisForm(
    version = "2.0",
    answers = Map(
      "purpose"                    -> List("INSTITUTIONAL"),
      "usesPersonalData"           -> Nil,
      "usesThirdPartyPersonalData" -> Nil,
      "usesConfidentialData"       -> Nil
    )
  )

  def validOnlySchemaManagementRiskAnalysisSeed(
    tenantKind: PersistentTenantKind
  ): PurposeManagement.RiskAnalysisFormSeed = {
    val validOnlySchemaRiskAnalysis =
      if (tenantKind == PersistentTenantKind.PA) validOnlySchemaRiskAnalysis2_0 else validOnlySchemaRiskAnalysis1_0
    RiskAnalysisValidation
      .validate(validOnlySchemaRiskAnalysis.toTemplate, true)(tenantKind.toTemplate)
      .toOption
      .get
      .toManagement(validOnlySchemaRiskAnalysis.riskAnalysisId)
  }

  def validOnlySchemaPersistentRiskAnalysis(tenantKind: PersistentTenantKind): PersistentRiskAnalysisForm = {
    val seed = validOnlySchemaManagementRiskAnalysisSeed(tenantKind)
    PersistentRiskAnalysisForm(
      id = UUID.randomUUID(),
      version = seed.version,
      riskAnalysisId = Some(UUID.randomUUID()),
      singleAnswers = seed.singleAnswers.map(a =>
        PersistentRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = a.key, value = a.value)
      ),
      multiAnswers = seed.multiAnswers.map(a =>
        PersistentRiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = a.key, values = a.values)
      )
    )
  }

  def validManagementRiskAnalysisSeed(tenantKind: PersistentTenantKind): PurposeManagement.RiskAnalysisFormSeed = {
    val riskAnalysis =
      if (tenantKind == PersistentTenantKind.PA) validRiskAnalysis3_0_Pa else validRiskAnalysis2_0_Private
    RiskAnalysisValidation
      .validate(riskAnalysis.toTemplate, false)(tenantKind.toTemplate)
      .toOption
      .get
      .toManagement(riskAnalysis.riskAnalysisId)
  }

  def validManagementRiskAnalysis(tenantKind: PersistentTenantKind): PurposeManagement.RiskAnalysisForm = {
    val seed = validManagementRiskAnalysisSeed(tenantKind)

    PurposeManagement.RiskAnalysisForm(
      id = UUID.randomUUID(),
      version = seed.version,
      singleAnswers = seed.singleAnswers.map(a =>
        PurposeManagement.RiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = a.key, value = a.value)
      ),
      multiAnswers = seed.multiAnswers.map(a =>
        PurposeManagement.RiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = a.key, values = a.values)
      )
    )
  }

  def validPersistentRiskAnalysis(tenantKind: PersistentTenantKind): PersistentRiskAnalysisForm = {
    val seed = validManagementRiskAnalysisSeed(tenantKind)

    PersistentRiskAnalysisForm(
      id = UUID.randomUUID(),
      riskAnalysisId = Some(UUID.randomUUID()),
      version = seed.version,
      singleAnswers = seed.singleAnswers.map(a =>
        PersistentRiskAnalysisSingleAnswer(id = UUID.randomUUID(), key = a.key, value = a.value)
      ),
      multiAnswers = seed.multiAnswers.map(a =>
        PersistentRiskAnalysisMultiAnswer(id = UUID.randomUUID(), key = a.key, values = a.values)
      )
    )
  }

  val purpose: PersistentPurpose = PersistentPurpose(
    id = UUID.randomUUID(),
    eserviceId = UUID.randomUUID(),
    consumerId = UUID.randomUUID(),
    versions = Seq.empty,
    suspendedByConsumer = None,
    suspendedByProducer = None,
    title = "A title",
    description = "A description",
    riskAnalysisForm = Some(validPersistentRiskAnalysis(PersistentTenantKind.PRIVATE)),
    createdAt = timestamp,
    updatedAt = None,
    isFreeOfCharge = false,
    freeOfChargeReason = None
  )

  val purposeVersionNotInDraftState: PersistentPurposeVersion = PersistentPurposeVersion(
    id = UUID.randomUUID(),
    state = PurposeActive,
    createdAt = timestamp,
    updatedAt = None,
    firstActivationAt = None,
    expectedApprovalDate = None,
    dailyCalls = 1000,
    riskAnalysis = None,
    suspendedAt = None
  )

  val purposeVersion: PersistentPurposeVersion = PersistentPurposeVersion(
    id = UUID.randomUUID(),
    state = Draft,
    createdAt = timestamp,
    updatedAt = None,
    firstActivationAt = None,
    expectedApprovalDate = None,
    dailyCalls = 1000,
    riskAnalysis = None,
    suspendedAt = None
  )

  val dependencyPurpose: PurposeManagement.Purpose = PurposeManagement.Purpose(
    id = UUID.randomUUID(),
    eserviceId = UUID.randomUUID(),
    consumerId = UUID.randomUUID(),
    versions = Seq.empty,
    suspendedByConsumer = None,
    suspendedByProducer = None,
    title = "A title",
    description = "A description",
    riskAnalysisForm = Some(validManagementRiskAnalysis(PersistentTenantKind.PRIVATE)),
    createdAt = timestamp,
    updatedAt = None,
    isFreeOfCharge = false,
    freeOfChargeReason = None
  )

  val dependencyPurposeVersion: PurposeManagement.PurposeVersion = PurposeManagement.PurposeVersion(
    id = UUID.randomUUID(),
    state = PurposeManagement.PurposeVersionState.DRAFT,
    createdAt = timestamp,
    updatedAt = None,
    firstActivationAt = None,
    expectedApprovalDate = None,
    dailyCalls = 1000,
    riskAnalysis = None,
    suspendedAt = None
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
    riskAnalysisForm = Some(validPersistentRiskAnalysis(PersistentTenantKind.PA)),
    createdAt = timestamp,
    updatedAt = None,
    isFreeOfCharge = false,
    freeOfChargeReason = None
  )

  def waitingForApprovalUpdate: WaitingForApprovalPurposeVersionUpdateContent =
    WaitingForApprovalPurposeVersionUpdateContent(timestamp)

  val purposes: Seq[PersistentPurpose] = Seq(purpose)

  val agreement: PersistentAgreement =
    PersistentAgreement(
      id = UUID.randomUUID(),
      eserviceId = UUID.randomUUID(),
      descriptorId = UUID.randomUUID(),
      producerId = UUID.randomUUID(),
      consumerId = UUID.randomUUID(),
      state = Active,
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
      stamps = PersistentStamps(),
      contract = None,
      rejectionReason = None,
      suspendedAt = None
    )

  val client: PersistentClient =
    PersistentClient(
      id = UUID.randomUUID(),
      consumerId = UUID.randomUUID(),
      name = "Client",
      description = None,
      purposes = Seq.empty,
      relationships = Set.empty,
      kind = Consumer,
      createdAt = timestamp
    )
}
