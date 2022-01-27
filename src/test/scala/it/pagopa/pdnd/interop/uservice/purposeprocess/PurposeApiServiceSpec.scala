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
  PurposeSeedConverter
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
        description = Some("A description")
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
        createdAt = timestamp,
        updatedAt = None
      )

      mockEServiceRetrieve(eServiceId)
      mockOrganizationRetrieve(consumerId)

      (mockPurposeManagementService
        .createPurpose(_: String)(_: PurposeManagementDependency.PurposeSeed))
        .expects(bearerToken, PurposeSeedConverter.apiToDependency(seed))
        .once()
        .returns(Future.successful(managementResponse))

      val expected: Purpose = PurposeConverter.dependencyToApi(managementResponse)

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
        description = Some("A description")
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
        description = Some("A description")
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
        description = Some("A description")
      )

      val purposeProblem: PurposeProblem = SpecData.purposeProblem.copy(status = 400)
      val expectedProblem: Problem       = purposemanagement.ProblemConverter.dependencyToApi(purposeProblem)

      mockEServiceRetrieve(eServiceId)
      mockOrganizationRetrieve(consumerId)

      (mockPurposeManagementService
        .createPurpose(_: String)(_: PurposeManagementDependency.PurposeSeed))
        .expects(bearerToken, PurposeSeedConverter.apiToDependency(seed))
        .once()
        .returns(
          Future
            .failed(PurposeApiError[PurposeProblem](SpecData.purposeProblem.status, "Some error", Some(purposeProblem)))
        )

      Get() ~> service.createPurpose(seed) ~> check {
        status shouldEqual StatusCodes.BadRequest
        responseAs[Problem] shouldEqual expectedProblem
      }
    }

  }

}
