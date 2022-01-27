package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import com.nimbusds.jwt.JWTClaimsSet
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.EService
import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.Organization
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, Purpose, Purposes}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  CatalogManagementService,
  PartyManagementService,
  PurposeManagementService
}
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import spray.json.DefaultJsonProtocol

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

trait SpecHelper extends SprayJsonSupport with DefaultJsonProtocol with MockFactory {

  final val bearerToken: String = "token"

  implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken)

  val mockPartyManagementService: PartyManagementService     = mock[PartyManagementService]
  val mockPurposeManagementService: PurposeManagementService = mock[PurposeManagementService]
  val mockCatalogManagementService: CatalogManagementService = mock[CatalogManagementService]

  val service: PurposeApiService =
    PurposeApiServiceImpl(mockCatalogManagementService, mockPartyManagementService, mockPurposeManagementService)(
      ExecutionContext.global
    )

  def mockSubject(uuid: String): Try[JWTClaimsSet] = Success(new JWTClaimsSet.Builder().subject(uuid).build())

  def mockEServiceRetrieve(eServiceId: UUID): CallHandler2[String, UUID, Future[EService]] =
    (mockCatalogManagementService
      .getEServiceById(_: String)(_: UUID))
      .expects(bearerToken, eServiceId)
      .once()
      .returns(Future.successful(SpecData.eService.copy(id = eServiceId)))

  def mockOrganizationRetrieve(organizationId: UUID): CallHandler2[String, UUID, Future[Organization]] =
    (mockPartyManagementService
      .getOrganizationById(_: String)(_: UUID))
      .expects(bearerToken, organizationId)
      .once()
      .returns(Future.successful(SpecData.organization.copy(id = organizationId)))

  implicit def fromResponseUnmarshallerPurpose: FromEntityUnmarshaller[Purpose] =
    sprayJsonUnmarshaller[Purpose]
  implicit def fromResponseUnmarshallerPurposes: FromEntityUnmarshaller[Purposes] =
    sprayJsonUnmarshaller[Purposes]
  implicit def fromResponseUnmarshallerProblem: FromEntityUnmarshaller[Problem] =
    sprayJsonUnmarshaller[Problem]
}
