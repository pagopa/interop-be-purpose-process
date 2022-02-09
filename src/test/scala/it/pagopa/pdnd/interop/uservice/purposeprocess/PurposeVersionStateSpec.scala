package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.pdnd.interop.commons.utils.UID
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.pdnd.interop.uservice.agreementmanagement.client.{model => AgreementManagement}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.{model => PartyManagement}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  ActivatePurposeVersionPayload,
  StateChangeDetails
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement.PurposeVersionConverter
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.PurposeApiMarshallerImpl
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, PurposeVersion}
import org.scalamock.handlers.CallHandler4
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json._

import java.io.File
import java.util.UUID
import scala.concurrent.Future

class PurposeVersionStateSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest {

  import PurposeApiMarshallerImpl._

  "Purpose version archive" should {
    "succeed" in {
      val userId     = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val updatedVersion = SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ARCHIVED)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))

      (mockPurposeManagementService
        .archivePurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails))
        .expects(
          bearerToken,
          purposeId,
          versionId,
          PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)
        )
        .once()
        .returns(Future.successful(updatedVersion))

      Get() ~> service.archivePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail if Purpose does not exist" in {
      val userId    = UUID.randomUUID()
      val purposeId = UUID.randomUUID()
      val versionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val purposeProblem: PurposeManagement.Problem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem                  = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: String)(_: UUID))
        .expects(bearerToken, purposeId)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.archivePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Consumer" in {
      val userId     = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))

      Get() ~> service.archivePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0007"
      }
    }

  }

  "Purpose version suspend" should {
    "succeed if user is a Consumer" in {
      val userId     = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val updatedVersion = SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.SUSPENDED)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))

      (mockPurposeManagementService
        .suspendPurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails))
        .expects(
          bearerToken,
          purposeId,
          versionId,
          PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)
        )
        .once()
        .returns(Future.successful(updatedVersion))

      Get() ~> service.suspendPurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed if user is a Producer" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val producerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val updatedVersion = SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.SUSPENDED)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId, eserviceId = eServiceId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))
      mockEServiceRetrieve(eServiceId, SpecData.eService.copy(producerId = producerId))
      mockRelationshipsRetrieve(userId, producerId, SpecData.relationships(userId, producerId))

      (mockPurposeManagementService
        .suspendPurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails))
        .expects(
          bearerToken,
          purposeId,
          versionId,
          PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.PRODUCER)
        )
        .once()
        .returns(Future.successful(updatedVersion))

      Get() ~> service.suspendPurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail if Purpose does not exist" in {
      val userId    = UUID.randomUUID()
      val purposeId = UUID.randomUUID()
      val versionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val purposeProblem: PurposeManagement.Problem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem                  = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: String)(_: UUID))
        .expects(bearerToken, purposeId)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.suspendPurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Consumer or a Producer" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val producerId = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val purpose  = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId)
      val eService = SpecData.eService.copy(producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))
      mockEServiceRetrieve(eServiceId, eService)
      mockRelationshipsRetrieve(userId, producerId, SpecData.relationships().copy(items = Seq.empty))

      Get() ~> service.suspendPurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0009"
      }
    }

  }

  "Purpose version activate" should {
    def mockDocumentCreation(): Unit = {
      val documentId = UUID.randomUUID()
      val tempFile   = File.createTempFile(UUID.randomUUID().toString, UUID.randomUUID().toString)
      tempFile.deleteOnExit()

      (() => mockUUIDSupplier.get).expects().returning(documentId).once()
      (mockPdfCreator
        .createDocument(_: String, _: PurposeManagement.RiskAnalysisForm, _: Int))
        .expects(*, *, *)
        .returning(Future.successful(tempFile))
        .once()

      ()
    }

    def mockFirstActivation(purposeId: UUID, versionId: UUID, result: PurposeManagement.PurposeVersion): Unit = {
      mockDocumentCreation()

      (() => mockDateTimeSupplier.get).expects().returning(SpecData.timestamp).once()

      (mockPurposeManagementService
        .activatePurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload))
        .expects(bearerToken, purposeId, versionId, *)
        .once()
        .returns(Future.successful(result))

      ()
    }

    def mockLoadValidation(
      activatingPurpose: PurposeManagement.Purpose,
      existingPurposes: PurposeManagement.Purposes,
      descriptorId: UUID
    ): Unit = {
      val producerId = UUID.randomUUID()
      mockPurposesRetrieve(
        eServiceId = Some(activatingPurpose.eserviceId),
        consumerId = Some(activatingPurpose.consumerId),
        states = Seq(PurposeManagement.PurposeVersionState.ACTIVE),
        result = existingPurposes
      )

      mockAgreementsRetrieve(
        activatingPurpose.eserviceId,
        activatingPurpose.consumerId,
        AgreementManagement.AgreementState.ACTIVE,
        Seq(
          SpecData.agreement.copy(
            consumerId = activatingPurpose.consumerId,
            eserviceId = activatingPurpose.eserviceId,
            descriptorId = descriptorId,
            producerId = producerId
          )
        )
      )
      ()
    }

    def mockWaitForApproval(
      purposeId: UUID,
      versionId: UUID,
      stateChangeDetails: PurposeManagement.StateChangeDetails,
      result: PurposeManagement.PurposeVersion
    ): CallHandler4[String, UUID, UUID, StateChangeDetails, Future[client.model.PurposeVersion]] = {
      (mockPurposeManagementService
        .waitForApprovalPurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails))
        .expects(bearerToken, purposeId, versionId, stateChangeDetails)
        .once()
        .returns(Future.successful(result))
    }

    def mockActivate(
      purposeId: UUID,
      versionId: UUID,
      result: PurposeManagement.PurposeVersion
    ): CallHandler4[String, UUID, UUID, ActivatePurposeVersionPayload, Future[client.model.PurposeVersion]] =
      (mockPurposeManagementService
        .activatePurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload))
        .expects(bearerToken, purposeId, versionId, *)
        .once()
        .returns(Future.successful(result))

    def mockAssertUserConsumer(userId: UUID, consumerId: UUID, result: PartyManagement.Relationships) =
      mockRelationshipsRetrieve(userId, consumerId, result)

    def mockAssertUserProducer(
      userId: UUID,
      consumerId: UUID,
      eService: CatalogManagement.EService,
      relationships: PartyManagement.Relationships
    ) = {
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))
      mockEServiceRetrieve(eService.id, eService)
      mockRelationshipsRetrieve(userId, eService.producerId, relationships)
    }

    "succeed from Draft when requested by Consumer if load not exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 1000
      )
      val purpose = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 2000
      )
      val version2_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 10000000
      )
      val purpose2 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1, version2_2))
      val purposes = SpecData.purposes.copy(purposes = Seq(purpose, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose)
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships(userId, consumerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockLoadValidation(purpose, purposes, descriptorId)
      mockFirstActivation(purposeId, versionId, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail from Draft when requested by Producer" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()
      val producerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version  = SpecData.purposeVersion.copy(id = versionId, state = PurposeManagement.PurposeVersionState.DRAFT)
      val purpose  = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))
      val eService = SpecData.eService.copy(id = eServiceId, producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)
      mockAssertUserProducer(userId, consumerId, eService, SpecData.relationships(userId, producerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0007"
      }
    }

    "succeed from Draft to Waiting For Approval if load exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose2 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1))
      val purposes = SpecData.purposes.copy(purposes = Seq(purpose1, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose1)
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships(userId, consumerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockLoadValidation(purpose1, purposes, descriptorId)
      mockWaitForApproval(purposeId, versionId, payload, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend when requested by Consumer if load not exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 1000
      )
      val purpose = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 2000
      )
      val version2_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 10000000
      )
      val purpose2 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1, version2_2))
      val purposes = SpecData.purposes.copy(purposes = Seq(purpose, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose)
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships(userId, consumerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockLoadValidation(purpose, purposes, descriptorId)
      mockActivate(purposeId, versionId, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend to Waiting for Approval when requested by Consumer if load exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose2 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1))
      val purposes = SpecData.purposes.copy(purposes = Seq(purpose1, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose1)
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships(userId, consumerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockLoadValidation(purpose1, purposes, descriptorId)
      mockWaitForApproval(purposeId, versionId, payload, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend when requested by Producer if load not exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose1)
      mockAssertUserProducer(userId, consumerId, eService, SpecData.relationships(userId, producerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockActivate(purposeId, versionId, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend when requested by Producer if load exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 8000
      )
      val purpose1 =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose1)
      mockAssertUserProducer(userId, consumerId, eService, SpecData.relationships(userId, producerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockActivate(purposeId, versionId, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail from Waiting for Approval when requested by Consumer" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships(userId, producerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0008"
      }
    }

    "succeed from Waiting for Approval when requested by Producer" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose)
      mockAssertUserProducer(userId, consumerId, eService, SpecData.relationships(userId, producerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockFirstActivation(purposeId, versionId, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Waiting for Approval to Waiting For Approval if load exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL,
        dailyCalls = 20000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose)
      mockAssertUserProducer(userId, consumerId, eService, SpecData.relationships(userId, producerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockFirstActivation(purposeId, versionId, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail from Archive when requested by Consumer" in {}
    "fail from Archive when requested by Producer" in {}

    "fail from Active when requested by Consumer" in {}
    "fail from Active when requested by Producer" in {}
  }
}
