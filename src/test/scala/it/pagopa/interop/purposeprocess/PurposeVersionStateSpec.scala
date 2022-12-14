package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagement}
import it.pagopa.interop.commons.utils.{ORGANIZATION_ID_CLAIM, USER_ROLES}
import it.pagopa.interop.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.interop.purposeprocess.api.converters._
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.PurposeVersionConverter
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiMarshallerImpl
import it.pagopa.interop.purposeprocess.model.{Problem, PurposeVersion}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json._

import java.util.UUID
import scala.concurrent.Future

class PurposeVersionStateSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest {

  import PurposeApiMarshallerImpl._

  "Purpose version archive" should {
    "succeed" in {

      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val updatedVersion = SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ARCHIVED)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.INACTIVE)

      (mockPurposeManagementService
        .archivePurposeVersion(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails)(_: Seq[(String, String)]))
        .expects(
          purposeId,
          versionId,
          PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER),
          *
        )
        .once()
        .returns(Future.successful(updatedVersion))

      Get() ~> service.archivePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail if Purpose does not exist" in {
      val purposeId = UUID.randomUUID()
      val versionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val purposeProblem: PurposeManagement.Problem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem                  = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                                  =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, *)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.archivePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Consumer" in {
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))

      Get() ~> service.archivePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0007"
      }
    }
  }

  "Purpose version suspend" should {
    "succeed if organization is a Consumer" in {
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val updatedVersion = SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.SUSPENDED)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.INACTIVE)

      (mockPurposeManagementService
        .suspendPurposeVersion(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails)(_: Seq[(String, String)]))
        .expects(
          purposeId,
          versionId,
          PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER),
          context
        )
        .once()
        .returns(Future.successful(updatedVersion))

      mockEServiceRetrieve(
        SpecData.purpose.eserviceId,
        SpecData.eService
          .copy(
            id = SpecData.purpose.eserviceId,
            producerId = SpecData.agreement.producerId,
            descriptors = Seq(SpecData.descriptor.copy(id = SpecData.agreement.descriptorId))
          )
      )

      Get() ~> service.suspendPurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed if organization is a Producer" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val producerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val updatedVersion = SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.SUSPENDED)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId, eserviceId = eServiceId))
      mockEServiceRetrieve(eServiceId, SpecData.eService.copy(producerId = producerId))
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.INACTIVE)

      (mockPurposeManagementService
        .suspendPurposeVersion(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails)(_: Seq[(String, String)]))
        .expects(
          purposeId,
          versionId,
          PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.PRODUCER),
          *
        )
        .once()
        .returns(Future.successful(updatedVersion))

      Get() ~> service.suspendPurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail if Purpose does not exist" in {
      val purposeId = UUID.randomUUID()
      val versionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val purposeProblem: PurposeManagement.Problem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem                  = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                                  =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, *)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.suspendPurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Consumer or a Producer" in {
      val eServiceId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val producerId = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> UUID.randomUUID().toString)

      val purpose  = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId)
      val eService = SpecData.eService.copy(producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)
      mockEServiceRetrieve(eServiceId, eService)

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
      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

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
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1, version2_2))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose, purposes, descriptorId)
      mockVersionFirstActivation(purposeId, versionId, eService.producerId, purpose.consumerId, updatedVersion)
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.ACTIVE)
      mockFileManagerStore("whateverPath")

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail from Draft when requested by Producer" in {

      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()
      val producerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version  = SpecData.purposeVersion.copy(id = versionId, state = PurposeManagement.PurposeVersionState.DRAFT)
      val purpose  = SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))
      val eService = SpecData.eService.copy(id = eServiceId, producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)
      mockEServiceRetrieve(eServiceId, eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0007"
      }
    }

    "succeed from Draft to Waiting For Approval if load exceeded" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose1, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload        = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose1)
      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose1, purposes, descriptorId)
      mockVersionWaitForApproval(purposeId, versionId, payload, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Draft to Waiting For Approval if total calls load exceeded and Producer is also Consumer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 1000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose1, purpose2))

      val descriptor =
        SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000, dailyCallsTotal = 8999)
      val eService   = SpecData.eService.copy(id = eServiceId, producerId = consumerId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload        = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose1)
      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose1, purposes, descriptorId)
      mockVersionWaitForApproval(purposeId, versionId, payload, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Draft to Waiting For Approval if load exceeded and Producer is also Consumer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose1, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload        = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose1)
      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose1, purposes, descriptorId)
      mockVersionWaitForApproval(purposeId, versionId, payload, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Draft to Waiting For Approval if total calls load exceeded" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 1000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose1, purpose2))

      val descriptor =
        SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000, dailyCallsTotal = 8999)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload        = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose1)
      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose1, purposes, descriptorId)
      mockVersionWaitForApproval(purposeId, versionId, payload, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend when requested by Consumer if load not exceeded" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

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
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1, version2_2))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion = version.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose, purposes, descriptorId)
      mockVersionActivate(purposeId, versionId, updatedVersion)
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.ACTIVE)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend to Waiting for Approval when requested by Consumer if load exceeded" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val version2_1 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose1, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
      val payload        = PurposeManagement.StateChangeDetails(changedBy = PurposeManagement.ChangedBy.CONSUMER)

      mockPurposeRetrieve(purposeId, purpose1)

      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose1, purposes, descriptorId)
      mockVersionWaitForApproval(purposeId, versionId, payload, updatedVersion)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend to Active if load exceeded and Producer is also Consumer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 1000)
      val eService   = SpecData.eService.copy(id = eServiceId, producerId = consumerId, descriptors = Seq(descriptor))

      val updatedVersion = version.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose1)

      mockEServiceRetrieve(eServiceId, eService)
      mockVersionActivate(purposeId, versionId, updatedVersion)
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.ACTIVE)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend when requested by Producer if load not exceeded" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      val updatedVersion = version.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose1)
      mockEServiceRetrieve(eServiceId, eService)
      mockVersionActivate(purposeId, versionId, updatedVersion)
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.ACTIVE)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend when requested by Producer if load exceeded" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version    = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.SUSPENDED,
        dailyCalls = 4000
      )
      val version1_2 = SpecData.purposeVersion.copy(
        id = UUID.randomUUID(),
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 8000
      )
      val purpose1   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version, version1_2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      val updatedVersion = version.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose1)
      mockEServiceRetrieve(eServiceId, eService)
      mockVersionActivate(purposeId, versionId, updatedVersion)
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.ACTIVE)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail from Waiting for Approval when requested by Consumer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eServiceId, eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0008"
      }
    }

    "succeed from Waiting for Approval when requested by Producer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      val updatedVersion =
        SpecData.purposeVersion.copy(state = PurposeManagement.PurposeVersionState.ACTIVE)

      mockPurposeRetrieve(purposeId, purpose)
      mockEServiceRetrieve(eServiceId, eService)
      mockVersionFirstActivation(purposeId, versionId, eService.producerId, purpose.consumerId, updatedVersion)
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.ACTIVE)
      mockFileManagerStore("whateverPath")

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "succeed from Suspend to Suspended" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

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
      val purpose2   =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version2_1, version2_2))
      val purposes   = SpecData.purposes.copy(purposes = Seq(purpose, purpose2))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      val updatedVersion = version

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose, purposes, descriptorId)
      mockVersionActivate(purposeId, versionId, updatedVersion)
      mockClientStateUpdate(purposeId, versionId, AuthorizationManagement.ClientComponentState.INACTIVE)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[PurposeVersion] shouldEqual PurposeVersionConverter.dependencyToApi(updatedVersion)
      }
    }

    "fail from Archive when requested by Consumer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.ARCHIVED,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eServiceId, eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0009"
      }
    }

    "fail from Archive when requested by Producer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.ARCHIVED,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)
      mockEServiceRetrieve(eServiceId, eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0009"
      }
    }

    "fail from Active when requested by Consumer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eServiceId, eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0009"
      }
    }

    "fail from Active when requested by Producer" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val producerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> producerId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.ACTIVE,
        dailyCalls = 4000
      )
      val purpose =
        SpecData.purpose.copy(eserviceId = eServiceId, consumerId = consumerId, versions = Seq(version))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor), producerId = producerId)

      mockPurposeRetrieve(purposeId, purpose)
      mockEServiceRetrieve(eServiceId, eService)

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0009"
      }
    }

    "fail on missing Risk Analysis Form" in {

      val eServiceId   = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val purposeId    = UUID.randomUUID()
      val versionId    = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()
      val documentId   = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", ORGANIZATION_ID_CLAIM -> consumerId.toString)

      val version = SpecData.purposeVersion.copy(
        id = versionId,
        state = PurposeManagement.PurposeVersionState.DRAFT,
        dailyCalls = 1000
      )
      val purpose = SpecData.purpose.copy(
        riskAnalysisForm = None,
        eserviceId = eServiceId,
        consumerId = consumerId,
        versions = Seq(version)
      )

      val purposes = SpecData.purposes.copy(purposes = Seq(purpose))

      val descriptor = SpecData.descriptor.copy(id = descriptorId, dailyCallsPerConsumer = 10000)
      val eService   = SpecData.eService.copy(id = eServiceId, descriptors = Seq(descriptor))

      mockPurposeRetrieve(purposeId, purpose)

      mockEServiceRetrieve(eServiceId, eService)
      mockVersionLoadValidation(purpose, purposes, descriptorId)
      mockOrganizationRetrieve(eService.producerId)
      mockOrganizationRetrieve(purpose.consumerId)
      (() => mockUUIDSupplier.get()).expects().returning(documentId).once()

      Get() ~> service.activatePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0016"
      }
    }
  }
}
