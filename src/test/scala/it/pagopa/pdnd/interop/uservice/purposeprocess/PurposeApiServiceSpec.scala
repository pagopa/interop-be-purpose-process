package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.pdnd.interop.commons.utils.UID
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Problem => PurposeProblem}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.uservice.purposeprocess.SpecData.timestamp
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.PurposeApiMarshallerImpl
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json._

import java.util.UUID
import scala.concurrent.Future

class PurposeApiServiceSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest {

  import PurposeApiMarshallerImpl._

  "Purpose creation" should {
    "succeed without risk analysis" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
//        riskAnalysisForm = SpecData.validRiskAnalysis
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
        .createPurpose(_: String)(_: PurposeManagementDependency.PurposeSeed))
        .expects(bearerToken, PurposeSeedConverter.apiToDependency(seed).toOption.get)
        .once()
        .returns(Future.successful(managementResponse))

      val expected: Purpose = PurposeConverter.dependencyToApi(managementResponse).toOption.get

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Purpose] shouldEqual expected
      }
    }

    "succeed with valid risk analysis" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

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
        .createPurpose(_: String)(_: PurposeManagementDependency.PurposeSeed))
        .expects(bearerToken, PurposeSeedConverter.apiToDependency(seed).toOption.get)
        .once()
        .returns(Future.successful(managementResponse))

      val expected: Purpose = PurposeConverter.dependencyToApi(managementResponse).toOption.get

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Purpose] shouldEqual expected
      }
    }

    "fail on incorrect risk analysis" in {
      val userId     = UUID.randomUUID()
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

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

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        //        riskAnalysisForm = SpecData.validRiskAnalysis
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

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = "A description",
        riskAnalysisForm = None
      )

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      mockAssertUserConsumer(userId, consumerId, SpecData.relationships())
      mockAgreementsRetrieve(eServiceId, consumerId)

      (mockPurposeManagementService
        .createPurpose(_: String)(_: PurposeManagementDependency.PurposeSeed))
        .expects(bearerToken, PurposeSeedConverter.apiToDependency(seed).toOption.get)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

  }

  "Purpose retrieve" should {
    "succeed" in {
      val purposeId = UUID.randomUUID()

      (mockPurposeManagementService
        .getPurpose(_: String)(_: UUID))
        .expects(bearerToken, purposeId)
        .once()
        .returns(Future.successful(SpecData.purpose))

      val expected: Purpose = PurposeConverter.dependencyToApi(SpecData.purpose).toOption.get

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purpose] shouldEqual expected
      }
    }

    "fail if Purpose does not exist" in {
      val purposeId = UUID.randomUUID()

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: String)(_: UUID))
        .expects(bearerToken, purposeId)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.getPurpose(purposeId.toString) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

  }

  "Purposes listing" should {
    "succeed" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val states = Seq(
        PurposeManagementDependency.PurposeVersionState.DRAFT,
        PurposeManagementDependency.PurposeVersionState.ACTIVE
      )

      (mockPurposeManagementService
        .getPurposes(_: String)(
          _: Option[UUID],
          _: Option[UUID],
          _: Seq[PurposeManagementDependency.PurposeVersionState]
        ))
        .expects(bearerToken, Some(eServiceId), Some(consumerId), states)
        .once()
        .returns(Future.successful(SpecData.purposes))

      val expected: Purposes = PurposesConverter.dependencyToApi(SpecData.purposes).toOption.get

      Get() ~> service.getPurposes(Some(eServiceId.toString), Some(consumerId.toString), "DRAFT,ACTIVE") ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purposes] shouldEqual expected
      }
    }

    "fail on Purpose management error" in {
      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurposes(_: String)(
          _: Option[UUID],
          _: Option[UUID],
          _: Seq[PurposeManagementDependency.PurposeVersionState]
        ))
        .expects(bearerToken, None, None, Seq.empty)
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.getPurposes(None, None, "") ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

  }

  "Purpose version creation" should {
    "succeed" in {
      val userId           = UUID.randomUUID()
      val consumerId       = UUID.randomUUID()
      val purposeId        = UUID.randomUUID()
      val purposeVersionId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

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
        .createPurposeVersion(_: String)(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed))
        .expects(bearerToken, purposeId, PurposeVersionSeedConverter.apiToDependency(seed))
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

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 404)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      (mockPurposeManagementService
        .getPurpose(_: String)(_: UUID))
        .expects(bearerToken, purposeId)
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

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

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

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val seed: PurposeVersionSeed = PurposeVersionSeed(dailyCalls = 100)

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)
      val apiError =
        PurposeApiError[String](purposeProblem.status, "Some error", Some(purposeProblem.toJson.prettyPrint))

      mockPurposeRetrieve(purposeId, SpecData.purpose.copy(consumerId = consumerId))
      mockRelationshipsRetrieve(userId, consumerId, SpecData.relationships(userId, consumerId))

      (mockPurposeManagementService
        .createPurposeVersion(_: String)(_: UUID, _: PurposeManagementDependency.PurposeVersionSeed))
        .expects(bearerToken, purposeId, PurposeVersionSeedConverter.apiToDependency(seed))
        .once()
        .returns(Future.failed(apiError))

      Get() ~> service.createPurposeVersion(purposeId.toString, seed) ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

  }

}
