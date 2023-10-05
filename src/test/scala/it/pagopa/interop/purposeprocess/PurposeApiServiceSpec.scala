package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.syntax.all._
import it.pagopa.interop.commons.utils.errors.{Problem => CommonProblem}
import it.pagopa.interop.commons.utils.{ORGANIZATION_ID_CLAIM, USER_ROLES}
import it.pagopa.interop.purposeprocess.api.Adapters._
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiMarshallerImpl._
import it.pagopa.interop.purposeprocess.api.impl.ResponseHandlers.serviceCode
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.{
  EServiceNotFound,
  PurposeNotFound,
  PurposeVersionDocumentNotFound,
  PurposeVersionNotFound
}
import it.pagopa.interop.purposeprocess.model._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.wordspec.AnyWordSpecLike
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.agreementmanagement.model.agreement.{Active => AgreementActive}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagementDependency}
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind
import it.pagopa.interop.purposemanagement.model.purpose.{
  Active,
  Archived,
  Draft,
  PersistentPurpose,
  PersistentPurposeVersion,
  PersistentPurposeVersionDocument,
  Suspended,
  WaitingForApproval
}
import it.pagopa.interop.catalogmanagement.model.{Deliver, Receive}
import it.pagopa.interop.commons.riskanalysis.api.impl.RiskAnalysisValidation

import java.time.{OffsetDateTime, ZoneOffset}
import org.scalatest.matchers.should.Matchers._

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class PurposeApiServiceSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest with ScalaFutures {

  "Purpose cloning" should {
    "succeed when there is only a WAITING FOR APPROVAL version" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeToClone = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PersistentPurposeVersion(
            id = UUID.randomUUID(),
            state = WaitingForApproval,
            createdAt = SpecData.timestamp,
            dailyCalls = 1000,
            updatedAt = None,
            firstActivationAt = None,
            suspendedAt = None,
            expectedApprovalDate = None,
            riskAnalysis = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PRIVATE)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(SpecData.dependencyPurposeVersion),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis(PersistentTenantKind.PRIVATE)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))
      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purposeToClone))

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(*, context)
        .once()
        .returns(Future.successful(purposeCloned))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }

    "succeed with multiple versions when latest is WAITING FOR APPROVAL" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeToClone = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PersistentPurposeVersion(
            id = UUID.randomUUID(),
            state = WaitingForApproval,
            createdAt = OffsetDateTime.of(2022, 12, 31, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 100,
            riskAnalysis = None,
            suspendedAt = None
          ),
          PersistentPurposeVersion(
            id = UUID.randomUUID(),
            state = Active,
            createdAt = OffsetDateTime.of(2022, 11, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 50,
            riskAnalysis = None,
            suspendedAt = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PRIVATE)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL,
            createdAt = OffsetDateTime.of(2022, 12, 31, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 100,
            riskAnalysis = None
          ),
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.ACTIVE,
            createdAt = OffsetDateTime.of(2022, 11, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 50,
            riskAnalysis = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis(PersistentTenantKind.PRIVATE)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purposeToClone))

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(*, context)
        .once()
        .returns(Future.successful(purposeCloned))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }

    "succeed with multiple versions when there is no WAITING FOR APPROVAL" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeToClone = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PersistentPurposeVersion(
            id = UUID.randomUUID(),
            state = Active,
            createdAt = OffsetDateTime.of(2022, 11, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 50,
            riskAnalysis = None,
            suspendedAt = None
          ),
          PersistentPurposeVersion(
            id = UUID.randomUUID(),
            state = Archived,
            createdAt = OffsetDateTime.of(2022, 12, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 100,
            riskAnalysis = None,
            suspendedAt = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.ACTIVE,
            createdAt = OffsetDateTime.of(2022, 11, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 100,
            riskAnalysis = None
          ),
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.ARCHIVED,
            createdAt = OffsetDateTime.of(2022, 12, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 50,
            riskAnalysis = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some))

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purposeToClone))

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(*, context)
        .once()
        .returns(Future.successful(purposeCloned))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }

    "succeed with check of new purpose title" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeToClone = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PersistentPurposeVersion(
            id = UUID.randomUUID(),
            state = Active,
            createdAt = OffsetDateTime.of(2022, 11, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 500,
            riskAnalysis = None,
            suspendedAt = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = None,
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.ACTIVE,
            createdAt = OffsetDateTime.of(2022, 11, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 500,
            riskAnalysis = None,
            suspendedAt = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = None,
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false
      )

      val purposeSeed = PurposeManagementDependency.PurposeSeed(
        eserviceId = purposeToClone.eserviceId,
        consumerId = purposeToClone.consumerId,
        riskAnalysisForm = None,
        title = s"${purposeToClone.title} - clone",
        description = purposeToClone.description,
        isFreeOfCharge = false,
        dailyCalls = 500
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purposeToClone))

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(purposeSeed, context)
        .once()
        .returns(Future.successful(purposeCloned))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }

    "fail if Purpose does not exist" in {

      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.failed(PurposeNotFound(purposeId)))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "fail if Purpose has empty states" in {
      val purposeId  = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeToCloneDraft = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purposeToCloneDraft))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Conflict
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Conflict.intValue
        problem.errors.head.code shouldBe "012-0014"
      }
    }

    "fail if Purpose has DRAFT state" in {
      val purposeId  = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeToCloneDraft = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PersistentPurposeVersion(
            id = UUID.randomUUID(),
            state = Draft,
            createdAt = SpecData.timestamp,
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 1000,
            riskAnalysis = None,
            suspendedAt = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purposeToCloneDraft))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Conflict
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Conflict.intValue
        problem.errors.head.code shouldBe "012-0014"
      }
    }
  }

  "Purpose creation" should {
    "succeed without risk analysis" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None,
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      val purposes: Seq[PersistentPurpose] = List.empty

      mockListPurposesRetrieve(purposes)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(seed.toManagement(false)(PersistentTenantKind.PRIVATE).toOption.get, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurpose.copy(id = purpose.id)))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purpose].id shouldEqual purpose.id
      }
    }

    "succeed with valid risk analysis" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = Some(SpecData.validRiskAnalysis2_0_Private),
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PRIVATE)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      val purposes: Seq[PersistentPurpose] = List.empty

      mockListPurposesRetrieve(purposes)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(seed.toManagement(false)(PersistentTenantKind.PRIVATE).toOption.get, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurpose.copy(id = purpose.id)))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purpose].id shouldEqual purpose.id
      }
    }

    "fail on SUSPENDED agreement " in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = Some(SpecData.validRiskAnalysis2_0_Private),
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive), Seq.empty)

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0005"
      }
    }

    "fail when is free of charge but without free of charge reason agreement " in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = Some(SpecData.validRiskAnalysis2_0_Private),
        isFreeOfCharge = true,
        dailyCalls = 100
      )

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0023"
      }
    }

    "fail on incorrect risk analysis" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val incorrectRiskAnalysis =
        RiskAnalysisForm(
          version = "1.0",
          answers = Map("purpose1" -> List("purpose"), "usesPersonalData" -> List("YES"))
        )

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = Some(incorrectRiskAnalysis),
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0004"
      }
    }

    "fail if Agreement does not exist" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None,
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive), Seq.empty)

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0005"
      }
    }

    "fail if User is not a Consumer" in {

      val eServiceId  = UUID.randomUUID()
      val consumerId  = UUID.randomUUID()
      val requesterId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> requesterId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None,
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }

    "fail if already exist a purpose with the same title" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None,
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = SpecData.persistentPurpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: Seq[PersistentPurpose] = Seq(purpose)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(consumerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some)))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      mockListPurposesRetrieve(purposes)

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Conflict
        responseAs[Problem].status shouldBe 409
      }
    }

    "succeed if there is no purpose with the same title" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None,
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val dependencySeed: PurposeManagementDependency.PurposeSeed = PurposeManagementDependency.PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None,
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose                          = SpecData.purpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: Seq[PersistentPurpose] = List.empty

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(consumerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some)))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      mockListPurposesRetrieve(purposes)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(dependencySeed, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurpose.copy(id = purpose.id)))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  "Purpose creation from EService" should {
    "succeed" in {
      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()
      val purposeId      = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          mode = Receive,
          riskAnalysis = Seq(SpecData.riskAnalysis.copy(id = riskAnalysisId))
        )
      )
      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      val purposeSeed: PurposeManagementDependency.PurposeSeed = seed.toManagement(
        eServiceId,
        SpecData.riskAnalysis.copy(id = riskAnalysisId).riskAnalysisForm.toManagement(seed.riskAnalysisId)
      )
      val purposes: Seq[PersistentPurpose]                     = List.empty

      mockListPurposesRetrieve(purposes)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(purposeSeed, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurpose.copy(id = purpose.id)))

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purpose].id shouldEqual purpose.id
      }
    }

    "fail if EService not found" in {
      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      (mockCatalogManagementService
        .getEServiceById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(eServiceId, *, *)
        .once()
        .returns(Future.failed(EServiceNotFound(eServiceId)))

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0024"
      }
    }

    "fail if EService has Deliver mode" in {
      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()
      val purposeId      = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          riskAnalysis = Seq(SpecData.riskAnalysis.copy(id = riskAnalysisId)),
          mode = Deliver
        )
      )

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0026"
      }
    }

    "fail if Risk Analysis not found" in {
      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()
      val purposeId      = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          mode = Receive,
          riskAnalysis = Seq.empty
        )
      )

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0027"
      }
    }

    "fail on SUSPENDED agreement " in {
      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val purposeId      = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          mode = Receive,
          riskAnalysis = Seq(SpecData.riskAnalysis.copy(id = riskAnalysisId))
        )
      )

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive), Seq.empty)

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0005"
      }
    }

    "fail when is free of charge but without free of charge reason agreement " in {
      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()
      val purposeId      = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = true,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = true,
        freeOfChargeReason = None
      )

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          mode = Receive,
          riskAnalysis = Seq(SpecData.riskAnalysis.copy(id = riskAnalysisId))
        )
      )

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0023"
      }
    }

    "fail if Agreement does not exist" in {

      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()
      val purposeId      = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = PersistentPurpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validPersistentRiskAnalysis(PersistentTenantKind.PA)),
        createdAt = SpecData.timestamp,
        updatedAt = None,
        isFreeOfCharge = false,
        freeOfChargeReason = None
      )

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          mode = Receive,
          riskAnalysis = Seq(SpecData.riskAnalysis.copy(id = riskAnalysisId))
        )
      )
      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive), Seq.empty)

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0005"
      }
    }

    "fail if User is not a Consumer" in {

      val eServiceId  = UUID.randomUUID()
      val consumerId  = UUID.randomUUID()
      val requesterId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> requesterId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = UUID.randomUUID(),
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }

    "fail if already exist a purpose with the same title" in {

      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose = SpecData.persistentPurpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: Seq[PersistentPurpose] = Seq(purpose)

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          mode = Receive,
          riskAnalysis = Seq(SpecData.riskAnalysis.copy(id = riskAnalysisId))
        )
      )

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(consumerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some)))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      mockListPurposesRetrieve(purposes)

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.Conflict
        responseAs[Problem].status shouldBe 409
      }
    }

    "succeed if there is no purpose with the same title" in {

      val eServiceId     = UUID.randomUUID()
      val consumerId     = UUID.randomUUID()
      val riskAnalysisId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: EServicePurposeSeed = EServicePurposeSeed(
        eServiceId = eServiceId,
        consumerId = consumerId,
        riskAnalysisId = riskAnalysisId,
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val dependencySeed: PurposeManagementDependency.PurposeSeed = PurposeManagementDependency.PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = Some(SpecData.riskAnalysisOnlySchemaSeed.copy(riskAnalysisId = Some(riskAnalysisId))),
        isFreeOfCharge = false,
        dailyCalls = 100
      )

      val purpose                          = SpecData.purpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: Seq[PersistentPurpose] = List.empty

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(
          descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)),
          mode = Receive,
          riskAnalysis = Seq(SpecData.riskAnalysisOnlySchema.copy(id = riskAnalysisId))
        )
      )

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(consumerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some)))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      mockListPurposesRetrieve(purposes)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(dependencySeed, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurpose.copy(id = purpose.id)))

      Get() ~> service.createPurposeFromEService(seed) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  "Purpose updating" should {
    "succeed if User is a Consumer and Purpose is in Draft State" in {

      val purposeId            = UUID.randomUUID()
      val eserviceId           = UUID.randomUUID()
      val consumerId           = UUID.randomUUID()
      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          riskAnalysisForm = None,
          dailyCalls = 100
        )
      val seed                 = PurposeManagementDependency.PurposeUpdateContent(
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        riskAnalysisForm = None,
        dailyCalls = 100
      )

      val purpose =
        SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(SpecData.purposeVersion))

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId))

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      mockPurposeUpdate(purposeId, seed, SpecData.dependencyPurpose.copy(id = purpose.id))

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
    "fail if case of eService with Receive mode" in {

      val purposeId            = UUID.randomUUID()
      val eserviceId           = UUID.randomUUID()
      val consumerId           = UUID.randomUUID()
      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          riskAnalysisForm = None,
          dailyCalls = 100
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purpose =
        SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(SpecData.purposeVersion))

      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId, mode = Receive))
      mockPurposeRetrieve(purposeId, purpose)

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0025"
      }
    }
    "fail when is free of charge but without free of charge reason agreement " in {
      val purposeId            = UUID.randomUUID()
      val eserviceId           = UUID.randomUUID()
      val consumerId           = UUID.randomUUID()
      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = true,
          riskAnalysisForm = None,
          dailyCalls = 100
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purpose =
        SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(SpecData.purposeVersion))

      mockPurposeRetrieve(purposeId, purpose)
      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId))

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0023"
      }
    }
    "fail if User is not a Consumer" in {

      val purposeId   = UUID.randomUUID()
      val consumerId  = UUID.randomUUID()
      val requesterId = UUID.randomUUID()
      val eserviceId  = UUID.randomUUID()

      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          riskAnalysisForm = None,
          dailyCalls = 100
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> requesterId.toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId))

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }
    "fail if Purpose is not in DRAFT state" in {
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()

      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          riskAnalysisForm = None,
          dailyCalls = 100
        )
      val purpose              =
        SpecData.purpose.copy(
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(SpecData.purposeVersionNotInDraftState)
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockPurposeRetrieve(purposeId, purpose)

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0015"
      }
    }
  }

  "Reverse Purpose updating" should {
    "succeed if User is a Consumer and Purpose is in Draft State" in {

      val purposeId                   = UUID.randomUUID()
      val eserviceId                  = UUID.randomUUID()
      val consumerId                  = UUID.randomUUID()
      val reversePurposeUpdateContent =
        ReversePurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          dailyCalls = 100
        )

      val purpose =
        SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(SpecData.purposeVersion))

      val seed = PurposeManagementDependency.PurposeUpdateContent(
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        riskAnalysisForm = purpose.riskAnalysisForm
          .traverse(risk =>
            RiskAnalysisValidation
              .validate(risk.toApi.toTemplate, schemaOnlyValidation = true)(PersistentTenantKind.PRIVATE.toTemplate)
              .toEither
              .map(_.toManagement(risk.riskAnalysisId))
          )
          .toOption
          .get,
        dailyCalls = 100
      )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId, mode = Receive))

      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      mockPurposeUpdate(purposeId, seed, SpecData.dependencyPurpose.copy(id = purpose.id))

      Post() ~> service.updateReversePurpose(purposeId.toString, reversePurposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
    "fail if case of eService with Deliver mode" in {

      val purposeId                   = UUID.randomUUID()
      val eserviceId                  = UUID.randomUUID()
      val consumerId                  = UUID.randomUUID()
      val reversePurposeUpdateContent =
        ReversePurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          dailyCalls = 100
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purpose =
        SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(SpecData.purposeVersion))

      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId, mode = Deliver))
      mockPurposeRetrieve(purposeId, purpose)

      Post() ~> service.updateReversePurpose(purposeId.toString, reversePurposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0026"
      }
    }
    "fail when is free of charge but without free of charge reason agreement " in {
      val purposeId                   = UUID.randomUUID()
      val eserviceId                  = UUID.randomUUID()
      val consumerId                  = UUID.randomUUID()
      val reversePurposeUpdateContent =
        ReversePurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = true,
          dailyCalls = 100
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purpose =
        SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(SpecData.purposeVersion))

      mockPurposeRetrieve(purposeId, purpose)
      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId, mode = Receive))

      Post() ~> service.updateReversePurpose(purposeId.toString, reversePurposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0023"
      }
    }
    "fail if User is not a Consumer" in {

      val purposeId   = UUID.randomUUID()
      val consumerId  = UUID.randomUUID()
      val requesterId = UUID.randomUUID()
      val eserviceId  = UUID.randomUUID()

      val reversePurposeUpdateContent =
        ReversePurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          dailyCalls = 100
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> requesterId.toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId))

      Post() ~> service.updateReversePurpose(purposeId.toString, reversePurposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }
    "fail if Purpose is not in DRAFT state" in {
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()

      val reversePurposeUpdateContent =
        ReversePurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          dailyCalls = 100
        )
      val purpose                     =
        SpecData.purpose.copy(
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(SpecData.purposeVersionNotInDraftState)
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockPurposeRetrieve(purposeId, purpose)

      Post() ~> service.updateReversePurpose(purposeId.toString, reversePurposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0015"
      }
    }
  }

  "Purpose retrieve" should {
    "succeed if requested by consumer" in {
      val purposeId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq(
          "bearer"              -> bearerToken,
          USER_ROLES            -> "admin",
          ORGANIZATION_ID_CLAIM -> SpecData.purpose.consumerId.toString
        )

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(SpecData.purpose))

      mockEServiceRetrieve(
        SpecData.purpose.eserviceId,
        SpecData.eService
          .copy(
            id = SpecData.purpose.eserviceId,
            producerId = SpecData.agreement.producerId,
            descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId))
          )
      )

      mockTenantRetrieve(
        SpecData.purpose.consumerId,
        SpecData.tenant.copy(id = SpecData.purpose.consumerId, kind = PersistentTenantKind.PRIVATE.some)
      )

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Purpose]
        response.id shouldEqual SpecData.purpose.id
        response.riskAnalysisForm should not be empty
      }
    }

    "succeed if requested by producer" in {
      val purposeId  = UUID.randomUUID()
      val producerId = UUID.randomUUID()

      val purpose = SpecData.purpose

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purpose))

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService
          .copy(
            producerId = producerId,
            descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId))
          )
      )

      mockTenantRetrieve(producerId, SpecData.tenant.copy(id = producerId, kind = PersistentTenantKind.PRIVATE.some))

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Purpose]
        response.id shouldEqual purpose.id
        response.riskAnalysisForm should not be empty
      }
    }

    "succeed and return empty risk analysis if User is not Producer or Consumer" in {
      val purposeId      = UUID.randomUUID()
      val purpose        = SpecData.purpose
      val organizationId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> organizationId.toString)

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purpose))

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)))
      )

      mockTenantRetrieve(
        organizationId,
        SpecData.tenant.copy(id = organizationId, kind = PersistentTenantKind.PRIVATE.some)
      )

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Purpose]
        response.id shouldEqual purpose.id
        response.riskAnalysisForm shouldBe empty
      }
    }

    "fail if Purpose does not exist" in {
      val purposeId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.failed(PurposeNotFound(purposeId)))

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }
  }

  "Purposes listing" should {
    "succeed" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val producerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purpose = SpecData.persistentPurpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: Seq[PersistentPurpose] = Seq(purpose)

      val states = Seq(
        PurposeManagementDependency.PurposeVersionState.DRAFT,
        PurposeManagementDependency.PurposeVersionState.ACTIVE
      )

      mockListPurposesRetrieve(purposes)

      Get() ~> service.getPurposes(
        Some("name"),
        eServiceId.toString,
        consumerId.toString,
        producerId.toString,
        states.mkString(","),
        false,
        0,
        10
      ) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purposes].results.map(_.id) should contain theSameElementsAs purposes.map(_.id)
      }
    }

  }

  "Purpose deletion" should {

    "succeed if there are no versions" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purpose =
        SpecData.purpose.copy(id = purposeId, eserviceId = eserviceId, consumerId = consumerId, versions = Seq.empty)

      mockPurposeRetrieve(purposeId, purpose)
      mockClientsRetrieve(purposeId, Seq.empty)
      mockPurposeDelete(purposeId)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "succeed if there is just one version in draft" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeDraftVersion =
        SpecData.purposeVersion.copy(state = Draft)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion)
        )

      mockPurposeRetrieve(purposeId, purpose)
      mockClientsRetrieve(purposeId, Seq.empty)
      mockPurposeDelete(purposeId)
      mockPurposeVersionDelete(purposeId, purposeDraftVersion.id)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "succeed if there is just one version in draft associated with a client" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeDraftVersion =
        SpecData.purposeVersion.copy(state = Draft)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion)
        )

      mockPurposeRetrieve(purposeId, purpose)
      mockClientsRetrieve(purposeId, Seq(SpecData.client))
      mockPurposeFromClientRemoval(purposeId, SpecData.client.id)
      mockPurposeDelete(purposeId)
      mockPurposeVersionDelete(purposeId, purposeDraftVersion.id)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "succeed if there is just one version in waiting for approval associated with a client" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeVersion =
        SpecData.purposeVersion.copy(state = WaitingForApproval)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, purpose)
      mockClientsRetrieve(purposeId, Seq(SpecData.client))
      mockPurposeFromClientRemoval(purposeId, SpecData.client.id)
      mockPurposeDelete(purposeId)
      mockPurposeVersionDelete(purposeId, purposeVersion.id)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "fail if the organization is not a consumer" in {
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val purposeVersion = SpecData.purposeVersion.copy(state = WaitingForApproval)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, purpose)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        responseAs[Problem].status shouldBe 403
        responseAs[Problem].errors.head.code shouldBe "012-0001"
      }
    }

    "fail if there is more than one version despite the state" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeDraftVersion    = SpecData.purposeVersion.copy(state = Draft)
      val purposeNonDraftVersion = SpecData.purposeVersion.copy(state = Active)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion, purposeNonDraftVersion)
        )

      mockPurposeRetrieve(purposeId, purpose)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Conflict
        responseAs[Problem].status shouldBe 409
        responseAs[Problem].errors.head.code shouldBe "012-0008"
      }
    }

    "fail if there is one version not in a deletable state" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeVersion = SpecData.purposeVersion.copy(state = Suspended)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, purpose)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Conflict
        responseAs[Problem].status shouldBe 409
        responseAs[Problem].errors.head.code shouldBe "012-0008"
      }
    }
  }

  "Purpose version deletion" should {

    "succeed" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(SpecData.purposeVersion.copy(id = versionId, state = Draft))
        )

      mockPurposeRetrieve(purposeId, purpose)
      mockPurposeVersionDelete(purposeId, versionId)

      Delete() ~> service.deletePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "fail if the organization is not a consumer" in {
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val purposeVersion = SpecData.purposeVersion.copy(state = WaitingForApproval)

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, purpose)

      Delete() ~> service.deletePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        responseAs[Problem].status shouldBe 403
        responseAs[Problem].errors.head.code shouldBe "012-0001"
      }
    }
  }

  "Purpose version creation" should {
    "succeed in case of version already published" in {

      val consumerId        = UUID.randomUUID()
      val documentId        = UUID.randomUUID()
      val purposeId         = UUID.randomUUID()
      val purposeVersionId1 = UUID.randomUUID()
      val purposeVersionId2 = UUID.randomUUID()
      val purposeVersionId3 = UUID.randomUUID()
      val eserviceId        = UUID.randomUUID()
      val descriptorId      = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version2_1 = SpecData.purposeVersion.copy(id = UUID.randomUUID(), state = Active, dailyCalls = 1000)
      val purpose2 = SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(version2_1))

      val path: String                               = "/here/there/foo/bar.pdf"
      val document: PersistentPurposeVersionDocument =
        PersistentPurposeVersionDocument(documentId, "application/pdf", path, SpecData.timestamp)

      val version1_1 = SpecData.purposeVersion.copy(id = purposeVersionId1, state = Active)
      val version1_2 = SpecData.purposeVersion.copy(
        id = purposeVersionId2,
        state = WaitingForApproval,
        riskAnalysis = Some(document),
        firstActivationAt = Some(SpecData.timestamp)
      )

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          versions = Seq(version1_1, version1_2),
          consumerId = consumerId,
          eserviceId = eserviceId
        )

      val purposes                 = Seq(purpose, purpose2)
      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 1000)

      val purposeVersion = PersistentPurposeVersion(
        id = purposeVersionId3,
        state = Active,
        createdAt = SpecData.timestamp,
        updatedAt = None,
        expectedApprovalDate = None,
        dailyCalls = seed.dailyCalls,
        riskAnalysis = Some(document),
        firstActivationAt = Some(SpecData.timestamp),
        suspendedAt = None
      )

      mockPurposeRetrieve(purposeId, purpose)

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeId, seed.toManagement, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurposeVersion.copy(id = purposeVersionId3)))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eserviceId, descriptors = Seq(descriptor))

      mockEServiceRetrieve(purpose.eserviceId, eService)

      mockVersionLoadValidation(purpose, purposes, descriptorId)

      val updatedVersion =
        SpecData.dependencyPurposeVersion.copy(
          id = purposeVersionId3,
          state = PurposeManagementDependency.PurposeVersionState.ACTIVE,
          riskAnalysis = Some(
            PurposeManagementDependency.PurposeVersionDocument(documentId, "application/pdf", path, SpecData.timestamp)
          ),
          firstActivationAt = Some(SpecData.timestamp)
        )

      mockVersionFirstActivation(purposeId, purposeVersionId3, eService.producerId, purpose.consumerId, updatedVersion)
      mockFileManagerStore("whateverPath")

      (() => mockDateTimeSupplier.get()).expects().returning(SpecData.timestamp).once()

      mockClientStateUpdate(purposeId, purposeVersionId3, AuthorizationManagementDependency.ClientComponentState.ACTIVE)

      val expected: PurposeVersion = purposeVersion.toApi

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual expected
      }
    }

    "succeed in case of version already published but goes to Waiting for Approval state" in {

      val consumerId        = UUID.randomUUID()
      val documentId        = UUID.randomUUID()
      val purposeId         = UUID.randomUUID()
      val purposeVersionId1 = UUID.randomUUID()
      val purposeVersionId2 = UUID.randomUUID()
      val purposeVersionId3 = UUID.randomUUID()
      val eserviceId        = UUID.randomUUID()
      val descriptorId      = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version2_1 = SpecData.purposeVersion.copy(id = UUID.randomUUID(), state = Active, dailyCalls = 1000)
      val purpose2 = SpecData.purpose.copy(eserviceId = eserviceId, consumerId = consumerId, versions = Seq(version2_1))

      val path: String                               = "/here/there/foo/bar.pdf"
      val document: PersistentPurposeVersionDocument =
        PersistentPurposeVersionDocument(documentId, "application/pdf", path, SpecData.timestamp)

      val version1_1 = SpecData.purposeVersion.copy(id = purposeVersionId1, state = Active)
      val version1_2 = SpecData.purposeVersion.copy(
        id = purposeVersionId2,
        state = WaitingForApproval,
        riskAnalysis = Some(document),
        firstActivationAt = Some(SpecData.timestamp)
      )

      val purpose =
        SpecData.purpose.copy(
          id = purposeId,
          versions = Seq(version1_1, version1_2),
          consumerId = consumerId,
          eserviceId = eserviceId
        )

      val purposes                 = Seq(purpose, purpose2)
      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 1000)

      val purposeVersion = PersistentPurposeVersion(
        id = purposeVersionId3,
        state = WaitingForApproval,
        createdAt = SpecData.timestamp,
        updatedAt = None,
        expectedApprovalDate = None,
        dailyCalls = seed.dailyCalls,
        riskAnalysis = None,
        firstActivationAt = Some(SpecData.timestamp),
        suspendedAt = None
      )

      mockPurposeRetrieve(purposeId, purpose)

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeId, seed.toManagement, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurposeVersion.copy(id = purposeVersionId3)))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 100)
      val eService   = SpecData.eService.copy(id = eserviceId, descriptors = Seq(descriptor))

      mockEServiceRetrieve(purpose.eserviceId, eService)

      mockVersionLoadValidation(purpose, purposes, descriptorId)

      val updatedVersion =
        SpecData.dependencyPurposeVersion.copy(
          id = purposeVersionId3,
          state = PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL,
          riskAnalysis = None,
          firstActivationAt = Some(SpecData.timestamp)
        )

      val payload = PurposeManagementDependency.StateChangeDetails(
        changedBy = PurposeManagementDependency.ChangedBy.CONSUMER,
        SpecData.timestamp
      )

      mockVersionWaitForApproval(purposeId, purposeVersionId3, payload, updatedVersion)

      (() => mockDateTimeSupplier.get()).expects().returning(SpecData.timestamp).once()

      val expected: PurposeVersion = purposeVersion.toApi

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual expected
      }
    }

    "fail if Purpose does not exist" in {

      val purposeId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.failed(PurposeNotFound(purposeId)))

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "fail if User is not a Consumer" in {

      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }

  }

  "Purpose waiting for approval version update" should {

    "succeed" in {

      val producerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()
      val eserviceId       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version  = SpecData.purposeVersion.copy(id = purposeVersionId)
      val expected =
        SpecData.dependencyPurposeVersion.copy(id = purposeVersionId, expectedApprovalDate = Some(SpecData.timestamp))

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(eserviceId = eserviceId, versions = Seq(version)))
      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId, producerId = producerId))

      (
        mockPurposeManagementService
          .updateWaitingForApprovalPurposeVersion(
            _: UUID,
            _: UUID,
            _: PurposeManagementDependency.WaitingForApprovalPurposeVersionUpdateContent
          )(_: Seq[(String, String)])
        )
        .expects(
          purposeId,
          purposeVersionId,
          PurposeManagementDependency.WaitingForApprovalPurposeVersionUpdateContent(SpecData.timestamp),
          context
        )
        .once()
        .returns(Future.successful[PurposeManagementDependency.PurposeVersion](expected))

      Post() ~> service.updateWaitingForApprovalPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.waitingForApprovalUpdate
      ) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual expected.toApi
      }
    }

    "fail if Purpose Version does not exist" in {

      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.failed(PurposeNotFound(purposeId)))

      Post() ~> service.updateWaitingForApprovalPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.waitingForApprovalUpdate
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.NotFound.intValue
        problem.errors.head.code shouldBe "012-0012"
      }
    }

    "fail if User is not a Producer" in {

      val producerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()
      val eserviceId       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(eserviceId = eserviceId))
      mockEServiceRetrieve(eserviceId, SpecData.eService.copy(id = eserviceId, producerId = producerId))

      Post() ~> service.updateWaitingForApprovalPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.waitingForApprovalUpdate
      ) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0002"
      }
    }
  }

  "Purpose Risk Analysis Configuration latest version retrieve" should {
    "succeed when Tenant kind is PA" in {

      val producerId: UUID = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(producerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = producerId, kind = PersistentTenantKind.PA.some)))

      Get() ~> service.retrieveLatestRiskAnalysisConfiguration(None) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "3.0"
      }
    }

    "succeed when Tenant kind is PRIVATE" in {

      val producerId: UUID = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(producerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = producerId, kind = PersistentTenantKind.PRIVATE.some)))

      Get() ~> service.retrieveLatestRiskAnalysisConfiguration(None) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "2.0"
      }
    }

    "succeed when Tenant kind is GSP" in {

      val producerId: UUID = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(producerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = producerId, kind = PersistentTenantKind.GSP.some)))

      Get() ~> service.retrieveLatestRiskAnalysisConfiguration(Some("GSP")) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "2.0"
      }
    }
  }

  "Purpose Risk Analysis Configuration for retrieving a specified version " should {
    "succeed when Tenant kind is PA" in {

      val producerId: UUID = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(producerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = producerId, kind = PersistentTenantKind.PA.some)))

      Get() ~> service.retrieveRiskAnalysisConfigurationByVersion(Some("PA"), "2.0") ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "2.0"
      }
    }

    "succeed when Tenant kind is PRIVATE" in {

      val producerId: UUID = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(producerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = producerId, kind = PersistentTenantKind.PRIVATE.some)))

      Get() ~> service.retrieveRiskAnalysisConfigurationByVersion(None, "1.0") ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "1.0"
      }
    }

    "succeed when Tenant kind is GSP" in {

      val producerId: UUID = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(producerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = producerId, kind = PersistentTenantKind.GSP.some)))

      Get() ~> service.retrieveRiskAnalysisConfigurationByVersion(None, "1.0") ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "1.0"
      }
    }
  }

  "Purpose Risk Analysis Document" should {
    "succeed if User is the Producer" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()
      val producerId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val path: String                               = "/here/there/foo/bar.pdf"
      val document: PersistentPurposeVersionDocument =
        PersistentPurposeVersionDocument(documentId, "application/pdf", path, OffsetDateTime.now())
      val purposeVersion = SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose        = SpecData.purpose.copy(versions = Seq(purposeVersion))

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purpose))

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService
          .copy(
            producerId = producerId,
            descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId))
          )
      )

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString,
        purposeVersionId.toString,
        documentId.toString
      ) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersionDocument] shouldEqual document.toApi
      }
    }

    "succeed if User is the Consumer" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      val path: String                               = "/here/there/foo/bar.pdf"
      val document: PersistentPurposeVersionDocument =
        PersistentPurposeVersionDocument(documentId, "application/pdf", path, OffsetDateTime.now())
      val purposeVersion = SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose        = SpecData.purpose.copy(versions = Seq(purposeVersion))

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> purpose.consumerId.toString)

      (mockPurposeManagementService
        .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(purposeId, *, *)
        .once()
        .returns(Future.successful(purpose))

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)))
      )

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString,
        purposeVersionId.toString,
        documentId.toString
      ) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersionDocument] shouldEqual document.toApi
      }
    }

    "fail if User is the Producer or the Consumer" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val path: String                               = "/here/there/foo/bar.pdf"
      val document: PersistentPurposeVersionDocument =
        PersistentPurposeVersionDocument(documentId, "application/pdf", path, OffsetDateTime.now())
      val purposeVersion = SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose        = SpecData.purpose.copy(versions = Seq(purposeVersion))

      mockPurposeRetrieve(purposeId, result = purpose)
      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)))
      )

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString,
        purposeVersionId.toString,
        documentId.toString
      ) ~> check {
        status shouldEqual StatusCodes.Forbidden
      }
    }

    "fail with a 404 if the document doesn't exist" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      val path: String = "/here/there/foo/bar.pdf"

      val document: PersistentPurposeVersionDocument =
        PersistentPurposeVersionDocument(UUID.randomUUID(), "application/pdf", path, OffsetDateTime.now())
      val purposeVersion                             =
        SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose                                    = SpecData.purpose.copy(versions = purposeVersion :: Nil)

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> purpose.consumerId.toString)

      mockPurposeRetrieve(purposeId, result = purpose)
      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)))
      )

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString,
        purposeVersionId.toString,
        documentId.toString
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[CommonProblem] shouldEqual CommonProblem(
          StatusCodes.NotFound,
          PurposeVersionDocumentNotFound(purposeId.toString, purposeVersionId.toString, documentId.toString),
          serviceCode,
          None
        )
      }
    }

    "fail with a 404 if the version doesn't exist" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      val purpose = SpecData.purpose.copy(versions = Seq(SpecData.purposeVersion))

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> purpose.consumerId.toString)

      mockPurposeRetrieve(purposeId, result = purpose)
      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)))
      )

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString,
        purposeVersionId.toString,
        documentId.toString
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[CommonProblem] shouldEqual CommonProblem(
          StatusCodes.NotFound,
          PurposeVersionNotFound(purposeId, purposeVersionId),
          serviceCode,
          None
        )
      }
    }

    "fail with a 404 if the purpose doesn't exist" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      mockPurposeRetrieveError(purposeId)

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString,
        purposeVersionId.toString,
        documentId.toString
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem]
      }
    }
  }
}
