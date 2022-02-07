package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import com.nimbusds.jwt.JWTClaimsSet
import com.typesafe.config.{Config, ConfigFactory}
import it.pagopa.pdnd.interop.commons.files.service.FileManager
import it.pagopa.pdnd.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.Relationships
import it.pagopa.pdnd.interop.uservice.partymanagement.client.{model => PartyManagement}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, Purpose, PurposeVersion, Purposes}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  CatalogManagementService,
  PDFCreator,
  PartyManagementService,
  PurposeManagementService
}
import org.scalamock.handlers.{CallHandler2, CallHandler3}
import org.scalamock.scalatest.MockFactory
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

trait SpecHelper extends SprayJsonSupport with DefaultJsonProtocol with MockFactory {

  final val bearerToken: String = "token"

  implicit val context: Seq[(String, String)] = Seq("bearer" -> bearerToken)

  val config: Config = ConfigFactory
    .parseResourcesAnySyntax("application-test")
    .resolve()
  val fileManagerType: String  = config.getString("pdnd-interop-commons.storage.type")
  val fileManager: FileManager = FileManager.getConcreteImplementation(fileManagerType).get

  val mockPartyManagementService: PartyManagementService     = mock[PartyManagementService]
  val mockPurposeManagementService: PurposeManagementService = mock[PurposeManagementService]
  val mockCatalogManagementService: CatalogManagementService = mock[CatalogManagementService]

  val mockPdfCreator: PDFCreator                   = mock[PDFCreator]
  val mockUUIDSupplier: UUIDSupplier               = mock[UUIDSupplier]
  val mockDateTimeSupplier: OffsetDateTimeSupplier = mock[OffsetDateTimeSupplier]

  val service: PurposeApiService =
    PurposeApiServiceImpl(
      mockCatalogManagementService,
      mockPartyManagementService,
      mockPurposeManagementService,
      fileManager,
      mockPdfCreator,
      mockUUIDSupplier,
      mockDateTimeSupplier
    )(ExecutionContext.global)

  def mockSubject(uuid: String): Try[JWTClaimsSet] = Success(new JWTClaimsSet.Builder().subject(uuid).build())

  def mockEServiceRetrieve(
    eServiceId: UUID,
    result: CatalogManagement.EService = SpecData.eService
  ): CallHandler2[String, UUID, Future[CatalogManagement.EService]] =
    (mockCatalogManagementService
      .getEServiceById(_: String)(_: UUID))
      .expects(bearerToken, eServiceId)
      .once()
      .returns(Future.successful(result.copy(id = eServiceId)))

  def mockOrganizationRetrieve(organizationId: UUID): CallHandler2[String, UUID, Future[PartyManagement.Organization]] =
    (mockPartyManagementService
      .getOrganizationById(_: String)(_: UUID))
      .expects(bearerToken, organizationId)
      .once()
      .returns(Future.successful(SpecData.organization.copy(id = organizationId)))

  def mockPurposeRetrieve(
    purposeId: UUID,
    result: PurposeManagement.Purpose = SpecData.purpose
  ): CallHandler2[String, UUID, Future[ManagementPurpose]] =
    (mockPurposeManagementService
      .getPurpose(_: String)(_: UUID))
      .expects(bearerToken, purposeId)
      .once()
      .returns(Future.successful(result.copy(id = purposeId)))

  def mockRelationshipsRetrieve(
    from: UUID,
    to: UUID,
    result: PartyManagement.Relationships = SpecData.relationships()
  ): CallHandler3[String, UUID, UUID, Future[Relationships]] =
    (mockPartyManagementService
      .getActiveRelationships(_: String)(_: UUID, _: UUID))
      .expects(bearerToken, from, to)
      .once()
      .returns(Future.successful(result))

  implicit def fromResponseUnmarshallerPurpose: FromEntityUnmarshaller[Purpose] =
    sprayJsonUnmarshaller[Purpose]
  implicit def fromResponseUnmarshallerPurposeVersion: FromEntityUnmarshaller[PurposeVersion] =
    sprayJsonUnmarshaller[PurposeVersion]
  implicit def fromResponseUnmarshallerPurposes: FromEntityUnmarshaller[Purposes] =
    sprayJsonUnmarshaller[Purposes]
  implicit def fromResponseUnmarshallerProblem: FromEntityUnmarshaller[Problem] =
    sprayJsonUnmarshaller[Problem]

  implicit def catalogProblemErrorFormat: RootJsonFormat[CatalogManagement.ProblemError] =
    jsonFormat2(CatalogManagement.ProblemError)
  implicit def catalogProblemFormat: RootJsonFormat[CatalogManagement.Problem] = jsonFormat5(CatalogManagement.Problem)
  implicit def partyProblemErrorFormat: RootJsonFormat[PartyManagement.ProblemError] =
    jsonFormat2(PartyManagement.ProblemError)
  implicit def partyProblemFormat: RootJsonFormat[PartyManagement.Problem] = jsonFormat5(PartyManagement.Problem)
  implicit def purposeProblemErrorFormat: RootJsonFormat[PurposeManagement.ProblemError] =
    jsonFormat2(PurposeManagement.ProblemError)
  implicit def purposeProblemFormat: RootJsonFormat[PurposeManagement.Problem] = jsonFormat5(PurposeManagement.Problem)

}
