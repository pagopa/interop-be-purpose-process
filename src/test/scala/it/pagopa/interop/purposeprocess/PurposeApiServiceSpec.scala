package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.interop.commons.utils.{UID, USER_ROLES}
import it.pagopa.interop.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.interop.purposemanagement.client.model.{Problem => PurposeProblem}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.interop.purposeprocess.SpecData.timestamp
import it.pagopa.interop.purposeprocess.api.converters._
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement._
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiMarshallerImpl
import it.pagopa.interop.purposeprocess.model._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json._

import java.util.UUID
import scala.concurrent.Future
import java.time.OffsetDateTime
import cats.implicits._
import akka.http.scaladsl.model.{ContentType, MediaTypes}
import org.scalatest.concurrent.ScalaFutures
import akka.util.ByteString
import scala.util.Random
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors
import it.pagopa.interop.purposeprocess.api.impl.problemOf

class PurposeApiServiceSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest with ScalaFutures {

  import PurposeApiMarshallerImpl._

  "Purpose creation" should {
    "succeed without risk analysis" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

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

      mockAssertUserConsumer(userId, consumerId, SpecData.relationships())
      mockAgreementsRetrieve(eServiceId, consumerId)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(PurposeSeedConverter.apiToDependency(seed).toOption.get, context)
        .once()
        .returns(Future.successful(managementResponse))

      mockPurposeEnhancement(managementResponse, isConsumer = true)

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Purpose].id shouldEqual managementResponse.id
      }
    }

    "succeed with valid risk analysis" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = Some(SpecData.validRiskAnalysis)
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

      mockAssertUserConsumer(userId, consumerId, SpecData.relationships())
      mockAgreementsRetrieve(eServiceId, consumerId)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(PurposeSeedConverter.apiToDependency(seed).toOption.get, context)
        .once()
        .returns(Future.successful(managementResponse))

      mockPurposeEnhancement(managementResponse, isConsumer = true)

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Purpose].id shouldEqual managementResponse.id
      }
    }

    "fail on incorrect risk analysis" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val incorrectRiskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = RiskAnalysisFormAnswers(purpose = "purpose", usesPersonalData = RiskAnalysisFormYesNoAnswer.YES)
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
        problem.errors.head.code shouldBe "012-0010"
      }
    }

    "fail if Agreement does not exist" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None
      )

      mockAssertUserConsumer(userId, consumerId, SpecData.relationships())
      mockAgreementsRetrieve(eServiceId, consumerId, Seq.empty)

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.BadRequest.intValue
        problem.errors.head.code shouldBe "012-0001"
      }
    }

    "fail on Purpose Management error" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None
      )

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      mockAssertUserConsumer(userId, consumerId, SpecData.relationships())
      mockAgreementsRetrieve(eServiceId, consumerId)

      (mockPurposeManagementService
        .createPurpose(_: PurposeManagementDependency.PurposeSeed)(_: Seq[(String, String)]))
        .expects(PurposeSeedConverter.apiToDependency(seed).toOption.get, context)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }
  }

  "Purpose retrieve" should {
    "succeed if requested by consumer" in {
      val purposeId = UUID.randomUUID()
      val userId    = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.successful(SpecData.purpose))

      mockAssertUserConsumer(userId, SpecData.purpose.consumerId, SpecData.relationships())
      mockPurposeEnhancement(SpecData.purpose, isConsumer = true)

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Purpose]
        response.id shouldEqual SpecData.purpose.id
        response.clients should not be empty
      }
    }

    "succeed if requested by producer" in {
      val purposeId = UUID.randomUUID()
      val userId    = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purpose  = SpecData.purpose
      val eService = SpecData.eService.copy(id = purpose.eserviceId, producerId = UUID.randomUUID())

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.successful(purpose))

      mockAssertUserProducerIfNotConsumer(userId, purpose.consumerId, eService, SpecData.relationships())
      mockPurposeEnhancement(purpose, isConsumer = false)

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Purpose]
        response.id shouldEqual purpose.id
        response.clients shouldBe empty
      }
    }

    "fail if Purpose does not exist" in {
      val purposeId = UUID.randomUUID()
      val userId    = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Consumer or a Producer" in {
      val purposeId = UUID.randomUUID()
      val userId    = UUID.randomUUID()

      val purpose  = SpecData.purpose
      val eService = SpecData.eService.copy(id = purpose.eserviceId, producerId = UUID.randomUUID())

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.successful(SpecData.purpose))

      mockAssertUserProducerIfNotConsumer(
        userId,
        SpecData.purpose.consumerId,
        eService,
        SpecData.relationships().copy(items = Seq.empty)
      )

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0009"
      }
    }
  }

  "Purposes listing" should {
    "succeed" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purpose = SpecData.purpose.copy(consumerId = consumerId, eserviceId = eServiceId)
      val purposes: PurposeManagementDependency.Purposes =
        PurposeManagementDependency.Purposes(Seq(purpose))

      val states = Seq(
        PurposeManagementDependency.PurposeVersionState.DRAFT,
        PurposeManagementDependency.PurposeVersionState.ACTIVE
      )

      (
        mockPurposeManagementService
          .getPurposes(_: Option[UUID], _: Option[UUID], _: Seq[PurposeManagementDependency.PurposeVersionState])(
            _: Seq[(String, String)]
          )
        )
        .expects(Some(eServiceId), Some(consumerId), states, context)
        .once()
        .returns(Future.successful(purposes))

      purposes.purposes.foreach { purpose =>
        mockAssertUserConsumer(userId, consumerId, SpecData.relationships())
        mockPurposeEnhancement(purpose, isConsumer = true)
      }

      Get() ~> service.getPurposes(Some(eServiceId.toString), Some(consumerId.toString), "DRAFT,ACTIVE") ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purposes].purposes.map(_.id) should contain theSameElementsAs purposes.purposes.map(_.id)
      }
    }

    "succeed showing only authorized purposes" in {
      val userId        = UUID.randomUUID()
      val ownEServiceId = UUID.randomUUID()
      val ownConsumerId = UUID.randomUUID()
      val ownProducerId = UUID.randomUUID()

      val purposeAsConsumerId   = UUID.randomUUID()
      val purposeAsProducerId   = UUID.randomUUID()
      val unauthorizedPurposeId = UUID.randomUUID()

      val otherConsumerId1 = UUID.randomUUID()
      val otherConsumerId2 = UUID.randomUUID()
      val otherEServiceId1 = UUID.randomUUID()
      val otherEServiceId2 = UUID.randomUUID()
      val otherProducerId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val ownEService         = SpecData.eService.copy(id = ownEServiceId, producerId = ownProducerId)
      val otherEService1      = SpecData.eService.copy(id = otherEServiceId1, producerId = otherProducerId)
      val purposeAsConsumer   =
        SpecData.purpose.copy(id = purposeAsConsumerId, consumerId = ownConsumerId, eserviceId = otherEServiceId2)
      val purposeAsProducer   =
        SpecData.purpose.copy(id = purposeAsProducerId, consumerId = otherConsumerId2, eserviceId = ownEServiceId)
      val purposeUnauthorized =
        SpecData.purpose.copy(id = unauthorizedPurposeId, consumerId = otherConsumerId1, eserviceId = otherEServiceId1)

      val purposes: PurposeManagementDependency.Purposes =
        PurposeManagementDependency.Purposes(Seq(purposeAsConsumer, purposeAsProducer, purposeUnauthorized))

      (
        mockPurposeManagementService
          .getPurposes(_: Option[UUID], _: Option[UUID], _: Seq[PurposeManagementDependency.PurposeVersionState])(
            _: Seq[(String, String)]
          )
        )
        .expects(None, None, Seq.empty, context)
        .once()
        .returns(Future.successful(purposes))

      // Consumer Purpose
      mockAssertUserConsumer(userId, ownConsumerId, SpecData.relationships(userId, ownConsumerId))
      mockPurposeEnhancement(purposeAsConsumer, isConsumer = true)
      // Producer Purpose
      mockAssertUserProducerIfNotConsumer(
        userId,
        otherConsumerId2,
        ownEService,
        SpecData.relationships(userId, ownProducerId)
      )
      mockPurposeEnhancement(purposeAsProducer, isConsumer = false, eService = Some(ownEService))
      // Purpose not allowed
      mockAssertUserProducerIfNotConsumer(
        userId,
        otherConsumerId1,
        otherEService1,
        SpecData.relationships().copy(items = Seq.empty)
      )

      Get() ~> service.getPurposes(None, None, "") ~> check {
        status shouldEqual StatusCodes.OK
        val result = responseAs[Purposes]
        result.purposes.map(_.id) should contain theSameElementsAs Seq(purposeAsConsumer.id, purposeAsProducer.id)
        result.purposes.find(_.id == purposeAsConsumer.id).get.clients should not be empty
        result.purposes.find(_.id == purposeAsProducer.id).get.clients shouldBe empty
      }
    }

    "fail on Purpose management error" in {
      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))
      val userId                         = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      (
        mockPurposeManagementService
          .getPurposes(_: Option[UUID], _: Option[UUID], _: Seq[PurposeManagementDependency.PurposeVersionState])(
            _: Seq[(String, String)]
          )
        )
        .expects(None, None, Seq.empty, context)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.getPurposes(None, None, "") ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }
  }

  "Purpose deletion" should {
    import PurposeManagementDependency.PurposeVersionState._

    "succeed if there are no versions" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val managementResponse =
        SpecData.purpose.copy(id = purposeId, eserviceId = eserviceId, consumerId = consumerId, versions = Seq.empty)

      val partyManagementResponse = SpecData.relationships(from = userId, to = consumerId)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq.empty)
      mockPurposeDelete(purposeId)
      mockRelationshipsRetrieve(userId, consumerId, partyManagementResponse)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "succeed if there is just one version in draft" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeDraftVersion =
        SpecData.purposeVersion.copy(state = PurposeManagementDependency.PurposeVersionState.DRAFT)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion)
        )

      val partyManagementResponse = SpecData.relationships(from = userId, to = consumerId)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq.empty)
      mockPurposeDelete(purposeId)
      mockPurposeVersionDelete(purposeId, purposeDraftVersion.id)
      mockRelationshipsRetrieve(userId, consumerId, partyManagementResponse)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "succeed if there is just one version in draft associated with a client" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeDraftVersion =
        SpecData.purposeVersion.copy(state = PurposeManagementDependency.PurposeVersionState.DRAFT)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion)
        )

      val partyManagementResponse = SpecData.relationships(from = userId, to = consumerId)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq(SpecData.client))
      mockPurposeFromClientRemoval(purposeId, SpecData.client.id)
      mockPurposeDelete(purposeId)
      mockPurposeVersionDelete(purposeId, purposeDraftVersion.id)
      mockRelationshipsRetrieve(userId, consumerId, partyManagementResponse)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "succeed if there is just one version in waiting for approval associated with a client" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeVersion =
        SpecData.purposeVersion.copy(state = PurposeManagementDependency.PurposeVersionState.WAITING_FOR_APPROVAL)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      val partyManagementResponse = SpecData.relationships(from = userId, to = consumerId)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockClientsRetrieve(Some(purposeId), Seq(SpecData.client))
      mockPurposeFromClientRemoval(purposeId, SpecData.client.id)
      mockPurposeDelete(purposeId)
      mockPurposeVersionDelete(purposeId, purposeVersion.id)
      mockRelationshipsRetrieve(userId, consumerId, partyManagementResponse)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "fail if the user is not a consumer" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeVersion = SpecData.purposeVersion.copy(state = WAITING_FOR_APPROVAL)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        responseAs[Problem].status shouldBe 403
        responseAs[Problem].errors.head.code shouldBe "012-0007"
      }
    }

    "fail if there is more than one version despite the state" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeDraftVersion    = SpecData.purposeVersion.copy(state = DRAFT)
      val purposeNonDraftVersion = SpecData.purposeVersion.copy(state = ACTIVE)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeDraftVersion, purposeNonDraftVersion)
        )

      val partyManagementResponse = SpecData.relationships(from = userId, to = consumerId)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockRelationshipsRetrieve(userId, consumerId, partyManagementResponse)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        responseAs[Problem].status shouldBe 403
        responseAs[Problem].errors.head.code shouldBe "012-0018"
      }
    }

    "fail if there is one version not in a deletable state" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeVersion = SpecData.purposeVersion.copy(state = SUSPENDED)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      val partyManagementResponse = SpecData.relationships(from = userId, to = consumerId)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockRelationshipsRetrieve(userId, consumerId, partyManagementResponse)

      Delete() ~> service.deletePurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        responseAs[Problem].status shouldBe 403
        responseAs[Problem].errors.head.code shouldBe "012-0018"
      }
    }
  }

  "Purpose version deletion" should {
    import PurposeManagementDependency.PurposeVersionState._

    "succeed" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val managementResponse =
        SpecData.purpose.copy(id = purposeId, eserviceId = eserviceId, consumerId = consumerId, versions = Seq.empty)

      mockPurposeRetrieve(purposeId, managementResponse)
      mockPurposeVersionDelete(purposeId, versionId)
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships(userId, consumerId))

      Delete() ~> service.deletePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[String] shouldBe empty
      }
    }

    "fail if the user is not a consumer" in {
      val userId     = UUID.randomUUID()
      val eserviceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeVersion = SpecData.purposeVersion.copy(state = WAITING_FOR_APPROVAL)

      val managementResponse =
        SpecData.purpose.copy(
          id = purposeId,
          eserviceId = eserviceId,
          consumerId = consumerId,
          versions = Seq(purposeVersion)
        )

      mockPurposeRetrieve(purposeId, managementResponse)
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))

      Delete() ~> service.deletePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.Forbidden
        responseAs[Problem].status shouldBe 403
        responseAs[Problem].errors.head.code shouldBe "012-0007"
      }
    }
  }

  "Purpose version creation" should {
    "succeed" in {
      val userId           = UUID.randomUUID()
      val consumerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

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
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))

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
      val userId    = UUID.randomUUID()
      val purposeId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Consumer" in {
      val userId     = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0007"
      }
    }

    "fail on Purpose Management error" in {
      val userId     = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))

      (mockPurposeManagementService
        .createPurposeVersion(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed)(_: Seq[(String, String)]))
        .expects(purposeId, PurposeVersionSeedConverter.apiToDependency(seed), context)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }
  }

  "Purpose draft version update" should {

    "succeed" in {
      val userId           = UUID.randomUUID()
      val consumerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val expected: PurposeManagementDependency.PurposeVersion = SpecData.purposeVersion.copy(dailyCalls = 100)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships(userId, consumerId))

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
      val userId           = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.failed(apiError))

      Post() ~> service.updateDraftPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.draftUpdate(100)
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Consumer" in {
      val userId           = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val consumerId       = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockAssertUserConsumer(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))

      Post() ~> service.updateDraftPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.draftUpdate(100)
      ) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0007"
      }
    }

    "fail on Purpose Management error" in {
      val userId           = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val consumerId       = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))

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
        .returns(Future.failed(apiError))

      Post() ~> service.updateDraftPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.draftUpdate(100)
      ) ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }
  }

  "Purpose waiting for approval version update" should {

    "succeed" in {
      val userId           = UUID.randomUUID()
      val producerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()
      val eserviceId       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val expected: PurposeManagementDependency.PurposeVersion =
        SpecData.purposeVersion.copy(expectedApprovalDate = Some(timestamp))

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(eserviceId = eserviceId))
      mockAssertUserProducer(
        userId,
        SpecData.eService.copy(id = eserviceId, producerId = producerId),
        SpecData.relationships(userId, producerId)
      )

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
      val userId           = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: UUID)(_: Seq[(String, String)]))
        .expects(purposeId, context)
        .once()
        .returns(Future.failed(apiError))

      Post() ~> service.updateWaitingForApprovalPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.waitingForApprovalUpdate
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if User is not a Producer" in {
      val userId           = UUID.randomUUID()
      val producerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()
      val eserviceId       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(eserviceId = eserviceId))
      mockAssertUserProducer(
        userId,
        SpecData.eService.copy(id = eserviceId, producerId = producerId),
        SpecData.relationships(userId, producerId).copy(items = Seq.empty)
      )

      Post() ~> service.updateWaitingForApprovalPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.waitingForApprovalUpdate
      ) ~> check {
        status shouldEqual StatusCodes.Forbidden
        val problem = responseAs[Problem]
        problem.status shouldBe StatusCodes.Forbidden.intValue
        problem.errors.head.code shouldBe "012-0008"
      }
    }

    "fail on Purpose Management error" in {
      val userId           = UUID.randomUUID()
      val producerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()
      val eserviceId       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError                       =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(eserviceId = eserviceId))
      mockAssertUserProducer(
        userId,
        SpecData.eService.copy(id = eserviceId, producerId = producerId),
        SpecData.relationships(userId, producerId)
      )

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
        .returns(Future.failed(apiError))

      Post() ~> service.updateWaitingForApprovalPurposeVersion(
        purposeId.toString,
        purposeVersionId.toString,
        SpecData.waitingForApprovalUpdate
      ) ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }
  }

  "Purpose Risk Analysys Document download" should {
    "succeed" in {
      val userId: UUID           = UUID.randomUUID()
      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val path: String                                                 = "/here/there/foo/bar.pdf"
      val document: PurposeManagementDependency.PurposeVersionDocument =
        PurposeManagementDependency.PurposeVersionDocument(documentId, "application/pdf", path, OffsetDateTime.now())
      val purposeVersion: PurposeManagementDependency.PurposeVersion   =
        SpecData.purposeVersion.copy(id = purposeVersionId, riskAnalysis = document.some)
      val purpose: PurposeManagementDependency.Purpose = SpecData.purpose.copy(versions = purposeVersion :: Nil)
      val emptyPdf: Array[Byte]                        = Random.nextBytes(1024)

      mockPurposeRetrieve(purposeId, result = purpose)
      mockFileManagerGet(path)(emptyPdf)

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString(),
        purposeVersionId.toString(),
        documentId.toString()
      ) ~> check {
        status shouldEqual StatusCodes.OK
        responseEntity.contentType shouldEqual ContentType(MediaTypes.`application/pdf`)
        responseAs[ByteString] shouldEqual ByteString(emptyPdf)
      }
    }

    "fail with a 404 if the document doesn't exist" in {
      val userId: UUID           = UUID.randomUUID()
      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

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

      mockPurposeRetrieve(purposeId, result = purpose)

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString(),
        purposeVersionId.toString(),
        documentId.toString()
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual problemOf(
          StatusCodes.NotFound,
          PurposeProcessErrors.PurposeVersionDocumentNotFound(
            purposeId.toString(),
            purposeVersionId.toString(),
            documentId.toString()
          )
        )
      }
    }

    "fail with a 404 if the version doesn't exist" in {
      val userId: UUID           = UUID.randomUUID()
      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      val purposeVersion: PurposeManagementDependency.PurposeVersion = SpecData.purposeVersion
      val purpose: PurposeManagementDependency.Purpose = SpecData.purpose.copy(versions = purposeVersion :: Nil)
      mockPurposeRetrieve(purposeId, result = purpose)

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString(),
        purposeVersionId.toString(),
        documentId.toString()
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual problemOf(
          StatusCodes.NotFound,
          PurposeProcessErrors.PurposeVersionNotFound(purposeId.toString(), purposeVersionId.toString())
        )
      }
    }

    "fail with a 404 if the purpose doesn't exist" in {
      val userId: UUID           = UUID.randomUUID()
      val purposeId: UUID        = UUID.randomUUID()
      val purposeVersionId: UUID = UUID.randomUUID()
      val documentId: UUID       = UUID.randomUUID()

      implicit val context: Seq[(String, String)] =
        Seq("bearer" -> bearerToken, USER_ROLES -> "admin", UID -> userId.toString)

      mockPurposeRetrieveError(SpecData.purposeProblem.copy(status = 404))

      Get() ~> service.getRiskAnalysisDocument(
        purposeId.toString(),
        purposeVersionId.toString(),
        documentId.toString()
      ) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem]
      }
    }
  }

}
