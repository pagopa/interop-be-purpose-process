package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.implicits._
import it.pagopa.interop.commons.utils.errors.{Problem => CommonProblem}
import it.pagopa.interop.commons.utils.{ORGANIZATION_ID_CLAIM, USER_ROLES}
import it.pagopa.interop.agreementmanagement.client.{model => AgreementManagementDependency}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.purposemanagement.model.purpose.PersistentPurpose
import it.pagopa.interop.purposeprocess.SpecData.timestamp
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement._
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiMarshallerImpl
import it.pagopa.interop.purposeprocess.api.impl.ResponseHandlers.serviceCode
import it.pagopa.interop.purposeprocess.common.readmodel.TotalCountResult
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.PurposeNotFound
import it.pagopa.interop.purposeprocess.model._
import org.mongodb.scala.bson.conversions.Bson
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json.JsonReader

import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class PurposeApiServiceSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest with ScalaFutures {

  import PurposeApiMarshallerImpl._

  "Purpose cloning" should {
    "succeed when there is only a WAITING FOR APPROVAL version" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val purposeToClone = PurposeManagementDependency.Purpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL,
            createdAt = timestamp,
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 1000,
            riskAnalysis = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(SpecData.purposeVersion),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        .returns(Future.successful(SpecData.purposeVersion))

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

      val purposeToClone = PurposeManagementDependency.Purpose(
        id = purposeId,
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
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(SpecData.purposeVersion),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        .returns(Future.successful(SpecData.purposeVersion))

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

      val purposeToClone = PurposeManagementDependency.Purpose(
        id = purposeId,
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
            dailyCalls = 50,
            riskAnalysis = None
          ),
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.ARCHIVED,
            createdAt = OffsetDateTime.of(2022, 12, 30, 11, 22, 33, 44, ZoneOffset.UTC),
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 100,
            riskAnalysis = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(SpecData.purposeVersion),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        .returns(Future.successful(SpecData.purposeVersion))

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

      val purposeToClone = PurposeManagementDependency.Purpose(
        id = purposeId,
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
            riskAnalysis = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = None,
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      val purposeCloned = PurposeManagementDependency.Purpose(
        id = UUID.randomUUID(),
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(SpecData.purposeVersion),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title - clone",
        description = "description",
        riskAnalysisForm = None,
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      val purposeSeed = PurposeManagementDependency.PurposeSeed(
        eserviceId = purposeToClone.eserviceId,
        consumerId = purposeToClone.consumerId,
        riskAnalysisForm = None,
        title = s"${purposeToClone.title} - clone",
        description = purposeToClone.description
      )

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        .returns(Future.successful(SpecData.purposeVersion))

      Get() ~> service.clonePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
      }
    }

    "fail if Purpose does not exist" in {

      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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

      val purposeToCloneDraft = PurposeManagementDependency.Purpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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

      val purposeToCloneDraft = PurposeManagementDependency.Purpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(
          PurposeManagementDependency.PurposeVersion(
            id = UUID.randomUUID(),
            state = PurposeManagementDependency.PurposeVersionState.DRAFT,
            createdAt = timestamp,
            updatedAt = None,
            firstActivationAt = None,
            expectedApprovalDate = None,
            dailyCalls = 1000,
            riskAnalysis = None
          )
        ),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "title",
        description = "description",
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        riskAnalysisForm = None
      )

      val managementResponse = PurposeManagementDependency.Purpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      mockAgreementsRetrieve(
        eServiceId,
        consumerId,
        Seq(AgreementManagementDependency.AgreementState.ACTIVE, AgreementManagementDependency.AgreementState.SUSPENDED)
      )

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(PurposeSeedConverter.apiToDependency(seed).toOption.get, context)
        .once()
        .returns(Future.successful(managementResponse))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Purpose].id shouldEqual managementResponse.id
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
        riskAnalysisForm = Some(SpecData.validRiskAnalysis1_0)
      )

      val managementResponse = PurposeManagementDependency.Purpose(
        id = purposeId,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(SpecData.validManagementRiskAnalysis),
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      mockAgreementsRetrieve(
        eServiceId,
        consumerId,
        Seq(AgreementManagementDependency.AgreementState.ACTIVE, AgreementManagementDependency.AgreementState.SUSPENDED)
      )

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(PurposeSeedConverter.apiToDependency(seed).toOption.get, context)
        .once()
        .returns(Future.successful(managementResponse))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Purpose].id shouldEqual managementResponse.id
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
          answers = Map("purpose" -> List("purpose"), "usesPersonalData" -> List("YES"))
        )

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = Some(incorrectRiskAnalysis)
      )

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
        riskAnalysisForm = None
      )

      mockAgreementsRetrieve(
        eServiceId,
        consumerId,
        Seq(
          AgreementManagementDependency.AgreementState.ACTIVE,
          AgreementManagementDependency.AgreementState.SUSPENDED
        ),
        Seq.empty
      )

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0005"
      }
    }

    "fail if User is not a Consumer" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None
      )

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0001"
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
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Purpose]
        response.id shouldEqual purpose.id
        response.riskAnalysisForm should not be empty
      }
    }

    "succeed and return empty risk analysis if User is not Producer or Consumer" in {
      val purposeId = UUID.randomUUID()
      val purpose   = SpecData.purpose

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.successful(purpose))

      mockEServiceRetrieve(
        purpose.eserviceId,
        SpecData.eService.copy(descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId)))
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
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        .returns(Future.successful(Seq(TotalCountResult(1))))

      Get() ~> service.getPurposes(
        Some("name"),
        eServiceId.toString,
        consumerId.toString,
        producerId.toString,
        states.mkString(","),
        0,
        10
      ) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purposes].results.map(_.id) should contain theSameElementsAs purposes.map(_.id)
      }
    }

  }

  "Purpose deletion" should {
    import PurposeManagementDependency.PurposeVersionState._

    "succeed if there are no versions" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val managementResponse =
        SpecData.purpose.copy(id = purposeId, eserviceId = eserviceId, consumerId = consumerId, versions = Seq.empty)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq.empty)
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
        SpecData.purposeVersion.copy(state = PurposeManagementDependency.PurposeVersionState.DRAFT)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq.empty)
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
        SpecData.purposeVersion.copy(state = PurposeManagementDependency.PurposeVersionState.DRAFT)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq(SpecData.client))
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
        SpecData.purposeVersion.copy(state = PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq(SpecData.client))
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

      val purposeVersion = SpecData.purposeVersion.copy(state = WAITING_FOR_APPROVAL)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)

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

      val purposeDraftVersion    = SpecData.purposeVersion.copy(state = DRAFT)
      val purposeNonDraftVersion = SpecData.purposeVersion.copy(state = ACTIVE)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion, purposeNonDraftVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)

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

      val purposeVersion = SpecData.purposeVersion.copy(state = SUSPENDED)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Conflict
        responseAs[Problem].status shouldBe 409
        responseAs[Problem].errors.head.code shouldBe "012-0008"
      }
    }
  }

  "Purpose version deletion" should {
    import PurposeManagementDependency.PurposeVersionState._

    "succeed" in {

      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(SpecData.purposeVersion.copy(id = versionId, state = DRAFT))
        )

      mockPurposeRetrieve(purposeId, managementResponse)
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

      val purposeVersion = SpecData.purposeVersion.copy(state = WAITING_FOR_APPROVAL)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)

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

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      val managementResponse = PurposeManagementDependency.PurposeVersion(
        id = purposeVersionId,
        state = PurposeManagementDependency.PurposeVersionState.DRAFT,
        createdAt = timestamp,
        updatedAt = None,
        expectedApprovalDate = None,
        dailyCalls = seed.dailyCalls,
        riskAnalysis = None
      )

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeId, PurposeVersionSeedConverter.apiToDependency(seed), context)
        .once()
        .returns(Future.successful(managementResponse))

      val expected: PurposeVersion = PurposeVersionConverter.dependencyToApi(managementResponse)

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[PurposeVersion] shouldEqual expected
      }
    }

    "fail if Purpose does not exist" in {

      val purposeId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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

      val version: PurposeManagementDependency.PurposeVersion  = SpecData.purposeVersion.copy(id = purposeVersionId)
      val expected: PurposeManagementDependency.PurposeVersion = version.copy(dailyCalls = 100)

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
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(expected)
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

  }

  "Purpose waiting for approval version update" should {

    "succeed" in {

      val producerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()
      val eserviceId       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version: PurposeManagementDependency.PurposeVersion  = SpecData.purposeVersion.copy(id = purposeVersionId)
      val expected: PurposeManagementDependency.PurposeVersion = version.copy(expectedApprovalDate = Some(timestamp))

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
          PurposeManagementDependency.WaitingForApprovalPurposeVersionUpdateContent(timestamp),
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
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(expected)
      }
    }

    "fail if Purpose Version does not exist" in {

      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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

  "Purpose Risk Analysis Document" should {
    "succeed if User is the Producer" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()
      val producerId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val path: String                                                 = "/here/there/foo/bar.pdf"
      val document: PurposeManagementDependency.PurposeVersionDocument =
        PurposeManagementDependency.PurposeVersionDocument(documentId, "application/pdf", path, OffsetDateTime.now())
      val purposeVersion: PurposeManagementDependency.PurposeVersion   =
        SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose: PurposeManagementDependency.Purpose = SpecData.purpose.copy(versions = Seq(purposeVersion))

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        responseAs[PurposeVersionDocument] shouldEqual PurposeVersionDocumentConverter.dependencyToApi(document)
      }
    }

    "succeed if User is the Consumer" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      val path: String                                                 = "/here/there/foo/bar.pdf"
      val document: PurposeManagementDependency.PurposeVersionDocument =
        PurposeManagementDependency.PurposeVersionDocument(documentId, "application/pdf", path, OffsetDateTime.now())
      val purposeVersion: PurposeManagementDependency.PurposeVersion   =
        SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose: PurposeManagementDependency.Purpose = SpecData.purpose.copy(versions = Seq(purposeVersion))

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> purpose.consumerId.toString)

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
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
        responseAs[PurposeVersionDocument] shouldEqual PurposeVersionDocumentConverter.dependencyToApi(document)
      }
    }

    "fail if User is the Producer or the Consumer" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val path: String                                                 = "/here/there/foo/bar.pdf"
      val document: PurposeManagementDependency.PurposeVersionDocument =
        PurposeManagementDependency.PurposeVersionDocument(documentId, "application/pdf", path, OffsetDateTime.now())
      val purposeVersion: PurposeManagementDependency.PurposeVersion   =
        SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose: PurposeManagementDependency.Purpose = SpecData.purpose.copy(versions = purposeVersion :: Nil)

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

      val path: String                                                 = "/here/there/foo/bar.pdf"
      val document: PurposeManagementDependency.PurposeVersionDocument =
        PurposeManagementDependency.PurposeVersionDocument(
          UUID.randomUUID(),
          "application/pdf",
          path,
          OffsetDateTime.now()
        )
      val purposeVersion: PurposeManagementDependency.PurposeVersion   =
        SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose: PurposeManagementDependency.Purpose = SpecData.purpose.copy(versions = purposeVersion :: Nil)

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
          PurposeProcessErrors
            .PurposeVersionDocumentNotFound(purposeId.toString, purposeVersionId.toString, documentId.toString),
          serviceCode,
          None
        )
      }
    }

    "fail with a 404 if the version doesn't exist" in {

      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      val purposeVersion: PurposeManagementDependency.PurposeVersion = SpecData.purposeVersion
      val purpose: PurposeManagementDependency.Purpose = SpecData.purpose.copy(versions = purposeVersion :: Nil)

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
          PurposeProcessErrors.PurposeVersionNotFound(purposeId, purposeVersionId),
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
