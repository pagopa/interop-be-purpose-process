package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import it.pagopa.pdnd.interop.commons.jwt.service.JWTReader
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement.{
  PurposeConverter,
  PurposeSeedConverter
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.{PurposeApiMarshallerImpl, PurposeApiServiceImpl}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  CatalogManagementService,
  PartyManagementService,
  PurposeManagementService
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class PurposeApiServiceSpec extends AnyWordSpecLike with MockFactory with SpecHelper with ScalatestRouteTest {

  import PurposeApiMarshallerImpl._

  val mockPartyManagementService: PartyManagementService     = mock[PartyManagementService]
  val mockPurposeManagementService: PurposeManagementService = mock[PurposeManagementService]
  val mockCatalogManagementService: CatalogManagementService = mock[CatalogManagementService]
  val mockJWTReader: JWTReader                               = mock[JWTReader]

  val service: PurposeApiService = PurposeApiServiceImpl(
    mockCatalogManagementService,
    mockPartyManagementService,
    mockPurposeManagementService,
    mockJWTReader
  )(ExecutionContext.global)

  implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken)

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

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(bearerToken, eServiceId)
        .once()
        .returns(Future.successful(SpecData.eService.copy(id = eServiceId)))

      (mockPartyManagementService
        .getOrganizationById(_: String)(_: UUID))
        .expects(bearerToken, consumerId)
        .once()
        .returns(Future.successful(SpecData.organization))

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

  }

}
