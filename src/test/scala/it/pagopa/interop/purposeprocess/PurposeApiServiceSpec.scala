package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.syntax.all._
import it.pagopa.interop.commons.utils.errors.{Problem => CommonProblem}
import it.pagopa.interop.commons.utils.{ORGANIZATION_ID_CLAIM, USER_ROLES}
import it.pagopa.interop.purposeprocess.api.Adapters._
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiMarshallerImpl._
import it.pagopa.interop.purposeprocess.api.impl.ResponseHandlers.serviceCode
import it.pagopa.interop.purposeprocess.common.readmodel.TotalCountResult
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.{
  PurposeNotFound,
  PurposeVersionDocumentNotFound,
  PurposeVersionNotFound
}
import it.pagopa.interop.purposeprocess.model._
import org.mongodb.scala.bson.conversions.Bson
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.wordspec.AnyWordSpecLike
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.agreementmanagement.model.agreement.{Active => AgreementActive}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind
import it.pagopa.interop.purposemanagement.model.purpose.{
  PersistentPurpose,
  PersistentPurposeVersion,
  WaitingForApproval,
  Active,
  Archived,
  Draft,
  Suspended,
  PersistentPurposeVersionDocument
}

import java.time.{OffsetDateTime, ZoneOffset}
import org.scalatest.matchers.should.Matchers._
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import spray.json.JsonReader

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

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeCloned.id, *, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurposeVersion.copy(id = purposeCloned.id)))

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

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeCloned.id, PurposeManagementDependency.PurposeVersionSeed(50, None), context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurposeVersion))

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

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeCloned.id, PurposeManagementDependency.PurposeVersionSeed(100, None), context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurposeVersion))

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
        .expects(purposeSeed, context)
        .once()
        .returns(Future.successful(purposeCloned))

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeCloned.id, PurposeManagementDependency.PurposeVersionSeed(500, None), context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurposeVersion))

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
        isFreeOfCharge = false
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

      // Data retrieve
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, 1, *, *)
        .once()
        .returns(Future.successful(purposes))
      // Total count
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, Int.MaxValue, *, *)
        .once()
        .returns(Future.successful(Seq(TotalCountResult(purposes.size))))

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
        riskAnalysisForm = Some(SpecData.validRiskAnalysis1_0_Private),
        isFreeOfCharge = false
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

      // Data retrieve
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, 1, *, *)
        .once()
        .returns(Future.successful(purposes))
      // Total count
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, Int.MaxValue, *, *)
        .once()
        .returns(Future.successful(Seq(TotalCountResult(purposes.size))))

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
        riskAnalysisForm = Some(SpecData.validRiskAnalysis1_0_Private),
        isFreeOfCharge = false
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
        riskAnalysisForm = Some(SpecData.validRiskAnalysis1_0_Private),
        isFreeOfCharge = true
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
        isFreeOfCharge = false
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
        isFreeOfCharge = false
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
        isFreeOfCharge = false
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
        isFreeOfCharge = false
      )

      val purpose = SpecData.persistentPurpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: Seq[PersistentPurpose] = Seq(purpose)

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(consumerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some)))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      // Data retrieve
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, 1, *, *)
        .once()
        .returns(Future.successful(purposes))
      // Total count
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, Int.MaxValue, *, *)
        .once()
        .returns(Future.successful(Seq(TotalCountResult(purposes.size))))

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
        isFreeOfCharge = false
      )

      val dependencySeed: PurposeManagementDependency.PurposeSeed = PurposeManagementDependency.PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None,
        isFreeOfCharge = false
      )

      val purpose                          = SpecData.purpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: Seq[PersistentPurpose] = List.empty

      (mockTenantManagementService
        .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
        .expects(consumerId, *, *)
        .once()
        .returns(Future.successful(SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PA.some)))

      mockAgreementsRetrieve(eServiceId, consumerId, Seq(AgreementActive))

      // Data retrieve
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, 1, *, *)
        .once()
        .returns(Future.successful(purposes))
      // Total count
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, Int.MaxValue, *, *)
        .once()
        .returns(Future.successful(Seq(TotalCountResult(purposes.size))))

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

  "Purpose updating" should {
    "succeed if User is a Consumer and Purpose is in Draft State" in {

      val purposeId            = UUID.randomUUID()
      val consumerId           = UUID.randomUUID()
      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          riskAnalysisForm = None
        )
      val seed                 = PurposeManagementDependency.PurposeUpdateContent(
        title = "A title",
        description = "A description",
        isFreeOfCharge = false,
        riskAnalysisForm = None
      )

      val purpose = SpecData.purpose.copy(consumerId = consumerId, versions = Seq(SpecData.purposeVersion))

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockPurposeRetrieve(purposeId, purpose)
      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      mockPurposeUpdate(purposeId, seed, SpecData.dependencyPurpose.copy(id = purpose.id))

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
    "fail when is free of charge but without free of charge reason agreement " in {
      val purposeId            = UUID.randomUUID()
      val consumerId           = UUID.randomUUID()
      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = true,
          riskAnalysisForm = None
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

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

      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          riskAnalysisForm = None
        )

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> requesterId.toString)

      mockTenantRetrieve(requesterId, SpecData.tenant.copy(id = requesterId, kind = PersistentTenantKind.PRIVATE.some))

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }
    "fail if Purpose is not in DRAFT state" in {
      val purposeId            = UUID.randomUUID()
      val consumerId           = UUID.randomUUID()
      val purposeUpdateContent =
        PurposeUpdateContent(
          title = "A title",
          description = "A description",
          isFreeOfCharge = false,
          riskAnalysisForm = None
        )
      val purpose              =
        SpecData.purpose.copy(consumerId = consumerId, versions = Seq(SpecData.purposeVersionNotInDraftState))

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockPurposeRetrieve(purposeId, purpose)
      mockTenantRetrieve(consumerId, SpecData.tenant.copy(id = consumerId, kind = PersistentTenantKind.PRIVATE.some))

      Post() ~> service.updatePurpose(purposeId.toString, purposeUpdateContent) ~> check {
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

      // Data retrieve
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, 10, *, *)
        .once()
        .returns(Future.successful(purposes))
      // Total count
      (mockReadModel
        .aggregate(_: String, _: Seq[Bson], _: Int, _: Int)(_: JsonReader[_], _: ExecutionContext))
        .expects("purposes", *, 0, Int.MaxValue, *, *)
        .once()
        .returns(Future.successful(Seq(TotalCountResult(purposes.size))))

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
    "succeed" in {

      val consumerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 1000)

      val purposeVersion = PersistentPurposeVersion(
        id = purposeVersionId,
        state = Draft,
        createdAt = SpecData.timestamp,
        updatedAt = None,
        expectedApprovalDate = None,
        dailyCalls = seed.dailyCalls,
        riskAnalysis = None,
        firstActivationAt = None,
        suspendedAt = None
      )

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeId, seed.toManagement, context)
        .once()
        .returns(Future.successful(SpecData.dependencyPurposeVersion.copy(id = purposeVersionId)))

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

  "Purpose draft version update" should {

    "succeed" in {

      val consumerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version  = SpecData.purposeVersion.copy(id = purposeVersionId)
      val expected = SpecData.dependencyPurposeVersion.copy(id = purposeVersionId, dailyCalls = 100)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId, versions = Seq(version)))

      (
        mockPurposeManagementService
          .updateDraftPurposeVersion(_: UUID, _: UUID, _: PurposeManagementDependency.DraftPurposeVersionUpdateContent)(
            _: Seq[(String, String)]
          )
        )
        .expects(
          purposeId,
          purposeVersionId,
          PurposeManagementDependency.DraftPurposeVersionUpdateContent(100),
          context
        )
        .once()
        .returns(Future.successful[PurposeManagementDependency.PurposeVersion](expected))

      Post() ~> service.updateDraftPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.draftUpdate(100)
      ) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual expected.toApi
      }
    }

    "fail if Purpose Version does not exist" in {

      val consumerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(id = purposeId, consumerId = consumerId))

      Post() ~> service.updateDraftPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.draftUpdate(100)
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.NotFound.intValue
        problem.errors.head.code shouldBe "012-0010"
      }
    }

    "fail if User is not a Consumer" in {

      val purposeId        = UUID.randomUUID()
      val consumerId       = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))

      Post() ~> service.updateDraftPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.draftUpdate(100)
      ) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }

    "fail if Purpose Version is not in DRAFT state" in {

      val consumerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version =
        SpecData.purposeVersion.copy(id = purposeVersionId, state = Active)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId, versions = Seq(version)))

      Post() ~> service.updateDraftPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.draftUpdate(100)
      ) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0016"
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

      Get() ~> service.retrieveLatestRiskAnalysisConfiguration() ~> check {
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

      Get() ~> service.retrieveLatestRiskAnalysisConfiguration() ~> check {
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

      Get() ~> service.retrieveLatestRiskAnalysisConfiguration() ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "1.0"
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

      Get() ~> service.retrieveRiskAnalysisConfigurationByVersion("1.0") ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[RiskAnalysisFormConfigResponse].version shouldEqual "1.0"
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

      Get() ~> service.retrieveRiskAnalysisConfigurationByVersion("1.0") ~> check {
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

      Get() ~> service.retrieveRiskAnalysisConfigurationByVersion("1.0") ~> check {
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
