package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.invoker.{ApiError => CatalogApiError}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.{Problem => CatalogProblem}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.invoker.{ApiError => PartyApiError}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.{Problem => PartyProblem}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Problem => PurposeProblem}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters._
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement.{
  PurposeConverter,
  PurposeSeedConverter,
  PurposesConverter
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.PurposeApiMarshallerImpl
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.concurrent.Future

class PurposeApiServiceSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest {

  import PurposeApiMarshallerImpl._

  "Purpose creation" should {
    "succeed" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = Some("A description"),
        riskAnalysisForm = SpecData.validRiskAnalysis
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
        riskAnalysisForm = SpecData.validManagementRiskAnalysis,
        createdAt = SpecData.timestamp,
        updatedAt = None
      )

      mockEServiceRetrieve(eServiceId)
      mockOrganizationRetrieve(consumerId)

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

    "fail if EService does not exist" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = Some("A description"),
        riskAnalysisForm = SpecData.validRiskAnalysis
      )

      val catalogProblem: CatalogProblem = SpecData.catalogProblem.copy(status = 404)
      val expectedProblem: Problem       = catalogmanagement.ProblemConverter.dependencyToApi(catalogProblem)

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(bearerToken, eServiceId)
        .once()
        .returns(
          Future
            .failed(CatalogApiError[CatalogProblem](SpecData.catalogProblem.status, "Some error", Some(catalogProblem)))
        )

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail if Consumer does not exist" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = Some("A description"),
        riskAnalysisForm = SpecData.validRiskAnalysis
      )

      val partyProblem: PartyProblem = SpecData.partyProblem.copy(status = 404)
      val expectedProblem: Problem   = partymanagement.ProblemConverter.dependencyToApi(partyProblem)

      mockEServiceRetrieve(eServiceId)

      (mockPartyManagementService
        .getOrganizationById(_: String)(_: UUID))
        .expects(bearerToken, consumerId)
        .once()
        .returns(
          Future
            .failed(PartyApiError[PartyProblem](SpecData.partyProblem.status, "Some error", Some(partyProblem)))
        )

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

    "fail on Purpose Management error" in {
      val eServiceId = UUID.randomUUID()
      val consumerId = UUID.randomUUID()

      val seed: PurposeSeed = PurposeSeed(
        eserviceId = eServiceId,
        consumerId = consumerId,
        title = "A title",
        description = Some("A description"),
        riskAnalysisForm = SpecData.validRiskAnalysis
      )

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 418)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)

      mockEServiceRetrieve(eServiceId)
      mockOrganizationRetrieve(consumerId)

      (mockPurposeManagementService
        .createPurpose(_: String)(_: PurposeManagementDependency.PurposeSeed))
        .expects(bearerToken, PurposeSeedConverter.apiToDependency(seed).toOption.get)
        .once()
        .returns(
          Future
            .failed(PurposeApiError[PurposeProblem](SpecData.purposeProblem.status, "Some error", Some(purposeProblem)))
        )

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

      (mockPurposeManagementService
        .getPurpose(_: String)(_: UUID))
        .expects(bearerToken, purposeId)
        .once()
        .returns(
          Future
            .failed(PurposeApiError[PurposeProblem](SpecData.purposeProblem.status, "Some error", Some(purposeProblem)))
        )

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

      (mockPurposeManagementService
        .getPurposes(_: String)(
          _: Option[UUID],
          _: Option[UUID],
          _: Seq[PurposeManagementDependency.PurposeVersionState]
        ))
        .expects(bearerToken, None, None, Seq.empty)
        .once()
        .returns(
          Future
            .failed(PurposeApiError[PurposeProblem](SpecData.purposeProblem.status, "Some error", Some(purposeProblem)))
        )

      Get() ~> service.getPurposes(None, None, "") ~> check {
        status shouldEqual StatusCodes.ImATeapot
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

  }

}
