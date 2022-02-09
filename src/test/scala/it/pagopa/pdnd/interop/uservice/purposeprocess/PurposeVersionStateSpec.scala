package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.pdnd.interop.commons.utils.UID
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.pdnd.interop.uservice.agreementmanagement.client.{model => AgreementManagement}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement.PurposeVersionConverter
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.PurposeApiMarshallerImpl
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, PurposeVersion}
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
    "succeed from Draft when requested by Consumer if load not exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()
      val documentId   = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))
      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 1000
      )
      val purpose = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))
      val otherVersion1 = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 2000
      )
      val otherVersion2 = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 10000000
      )
      val otherPurpose =
        SpecData.purpose.copy(
          eserviceId = eServiceId,
          consumerId = consumerId,
          versions = Seq(otherVersion1, otherVersion2)
        )
      val purposes = SpecData.purposes.copy(purposes = Seq(purpose, otherPurpose))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

//      val payload = PurposeManagement.ActivatePurposeVersionPayload(
//        riskAnalysis = None,
//        stateChangeDetails = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)
//      )

      val tempFile = File.createTempFile(UUID.randomUUID().toString, UUID.randomUUID().toString)
      tempFile.deleteOnExit()

      (() => mockUUIDSupplier.get).expects().returning(documentId).once()
      (() => mockDateTimeSupplier.get).expects().returning(SpecData.timestamp).once()
      (mockPdfCreator
        .createDocument(_: String, _: PurposeManagement.RiskAnalysisForm, _: Int))
        .expects(*, *, *)
        .returning(Future.successful(tempFile))
        .once()

      mockPurposeRetrieve(purposeId, purpose)
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockPurposesRetrieve(
        eServiceId = Some(purpose.eserviceId),
        consumerId = Some(purpose.consumerId),
        states = Seq(PurposeManagement.PurposeVersionState.ACTIVE),
        result = purposes
      )
      mockAgreementsRetrieve(
        eServiceId,
        consumerId,
        AgreementManagement.AgreementState.ACTIVE,
        Seq(
          SpecData.agreement.copy(
            consumerId = consumerId,
            eserviceId = eServiceId,
            descriptorId = descriptorId,
            producerId = producerId
          )
        )
      )

      (mockPurposeManagementService
        .activatePurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload))
        .expects(bearerToken, purposeId, versionId, *)
        .once()
        .returns(Future.successful(updatedVersion))

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }

//      val userId     = UUID.randomUUID()
//      val consumerId = UUID.randomUUID()
//      val purposeId  = UUID.randomUUID()
//      val versionId  = UUID.randomUUID()
//      val eServiceId = UUID.randomUUID()
//
//      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)
//
//      val eService = SpecData.eService.copy(id = eServiceId /*, dailyCalls = 10000*/ ) // TODO
//      val version  = SpecData.purposeVersion.copy(id = versionId)
//      val purpose  = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))
//
//      val updatedVersion = SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)
//      val payload = PurposeManagement.ActivatePurposeVersionPayload(
//        riskAnalysis = None,
//        stateChangeDetails = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)
//      )
//      mockPurposeRetrieve(purposeId, purpose)
//      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))
//      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
//
//      (mockPurposeManagementService
//        .activatePurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload))
//        .expects(bearerToken, purposeId, versionId, payload)
//        .once()
//        .returns(Future.successful(updatedVersion))
//
//      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
//        status shouldEqual StatusCodes.OK
//        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
//      }
    }

    "fail from Draft when requested by Producer" in {}

    "succeed from Draft to Waiting For Approval if load exceeded" in {
      val userId       = UUID.randomUUID()
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsMaxNumber = 1)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))
      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 1000
      )
      val purpose = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))
      val otherVersion = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 1000000
      )
      val otherPurpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(otherVersion))
      val purposes = SpecData.purposes.copy(purposes = Seq(purpose, otherPurpose))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose)
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))
      mockEServiceRetrieve(eServiceId = eServiceId, result = eService)
      mockPurposesRetrieve(
        eServiceId = Some(purpose.eserviceId),
        consumerId = Some(purpose.consumerId),
        states = Seq(PurposeManagement.PurposeVersionState.ACTIVE),
        result = purposes
      )
      mockAgreementsRetrieve(
        eServiceId,
        consumerId,
        AgreementManagement.AgreementState.ACTIVE,
        Seq(
          SpecData.agreement.copy(
            consumerId = consumerId,
            eserviceId = eServiceId,
            descriptorId = descriptorId,
            producerId = producerId
          )
        )
      )

      (mockPurposeManagementService
        .waitForApprovalPurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails))
        .expects(bearerToken, purposeId, versionId, payload)
        .once()
        .returns(Future.successful(updatedVersion))

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend when requested by Consumer if load not exceeded" in {}
    "succeed from Suspend to Waiting for Approval when requested by Consumer if load exceeded" in {}

    "succeed from Suspend when requested by Producer if load not exceeded" in {}
    "succeed from Suspend when requested by Producer if load exceeded" in {}

    "fail from Waiting for Approval when requested by Consumer" in {}
    "succeed from Waiting for Approval when requested by Producer" in {}
    "succeed from Waiting for Approval to Waiting For Approval if load exceeded" in {}

    "fail from Archive when requested by Consumer" in {}
    "fail from Archive when requested by Producer" in {}

    "fail from Active when requested by Consumer" in {}
    "fail from Active when requested by Producer" in {}
  }
}
