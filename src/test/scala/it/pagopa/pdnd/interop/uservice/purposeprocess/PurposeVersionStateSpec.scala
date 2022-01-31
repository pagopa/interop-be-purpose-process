package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.pdnd.interop.commons.utils.UID
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.PurposeApiMarshallerImpl
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.concurrent.Future

class PurposeVersionStateSpec extends AnyWordSpecLike with SpecHelper with ScalatestRouteTest {

  import PurposeApiMarshallerImpl._

  "Purpose version archive" should {
    "succeed" in {
      val userId = UUID.randomUUID()

      implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken, UID -> userId.toString)

      val consumerId = UUID.randomUUID()
      val purposeId  = UUID.randomUUID()
      val versionId  = UUID.randomUUID()

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
        .returns(Future.successful(()))

      Get() ~> service.archivePurposeVersion(purposeId.toString, versionId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
        responseAs[Option[String]] shouldEqual Some("")
      }
    }

//    "fail if EService does not exist" in {
//      val eServiceId = UUID.randomUUID()
//      val consumerId = UUID.randomUUID()
//
//      val seed: PurposeSeed = PurposeSeed(
//        eserviceId = eServiceId,
//        consumerId = consumerId,
//        title = "A title",
//        description = Some("A description")
//      )
//
//      val catalogProblem: CatalogProblem = SpecData.catalogProblem.copy(status = 404)
//      val expectedProblem: Problem       = catalogmanagement.ProblemConverter.dependencyToApi(catalogProblem)
//
//      (mockCatalogManagementService
//        .getEServiceById(_: String)(_: UUID))
//        .expects(bearerToken, eServiceId)
//        .once()
//        .returns(
//          Future
//            .failed(CatalogApiError[CatalogProblem](SpecData.catalogProblem.status, "Some error", Some(catalogProblem)))
//        )
//
//      Get() ~> service.createPurpose(seed) ~> check {
//        status shouldEqual StatusCodes.NotFound
//        responseAs[Problem] shouldEqual expectedProblem
//      }
//    }

  }

}
