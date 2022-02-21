package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import com.nimbusds.jwt.JWTClaimsSet
import com.typesafe.config.{Config, ConfigFactory}
import it.pagopa.pdnd.interop.commons.files.service.FileManager
import it.pagopa.pdnd.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.pdnd.interop.uservice.agreementmanagement.client.model.Agreement
import it.pagopa.pdnd.interop.uservice.agreementmanagement.client.{model => AgreementManagement}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.pdnd.interop.uservice.partymanagement.client.model.Relationships
import it.pagopa.pdnd.interop.uservice.partymanagement.client.{model => PartyManagement}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagement}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  ActivatePurposeVersionPayload,
  PurposeVersionState,
  RiskAnalysisForm,
  StateChangeDetails
}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.PurposeApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Problem, Purpose, PurposeVersion, Purposes}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service._
import org.scalamock.handlers.{CallHandler2, CallHandler3, CallHandler4}
import org.scalamock.scalatest.MockFactory
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.io.File
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

  val mockAgreementManagementService: AgreementManagementService         = mock[AgreementManagementService]
  val mockAuthorizationManagementService: AuthorizationManagementService = mock[AuthorizationManagementService]
  val mockPartyManagementService: PartyManagementService                 = mock[PartyManagementService]
  val mockPurposeManagementService: PurposeManagementService             = mock[PurposeManagementService]
  val mockCatalogManagementService: CatalogManagementService             = mock[CatalogManagementService]

  val mockPdfCreator: PDFCreator                   = mock[PDFCreator]
  val mockUUIDSupplier: UUIDSupplier               = mock[UUIDSupplier]
  val mockDateTimeSupplier: OffsetDateTimeSupplier = mock[OffsetDateTimeSupplier]

  val service: PurposeApiService =
    PurposeApiServiceImpl(
      mockAgreementManagementService,
      mockAuthorizationManagementService,
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

  def mockPurposesRetrieve(
    eServiceId: Option[UUID] = None,
    consumerId: Option[UUID] = None,
    states: Seq[PurposeManagement.PurposeVersionState] = Seq.empty,
    result: PurposeManagement.Purposes = SpecData.purposes
  ): CallHandler4[String, Option[UUID], Option[UUID], Seq[PurposeVersionState], Future[client.model.Purposes]] =
    (mockPurposeManagementService
      .getPurposes(_: String)(_: Option[UUID], _: Option[UUID], _: Seq[PurposeManagement.PurposeVersionState]))
      .expects(bearerToken, eServiceId, consumerId, states)
      .once()
      .returns(Future.successful(result))

  def mockAgreementsRetrieve(
    eServiceId: UUID,
    consumerId: UUID,
    result: Seq[AgreementManagement.Agreement] = Seq(SpecData.agreement)
  ): CallHandler3[String, UUID, UUID, Future[Seq[Agreement]]] =
    (mockAgreementManagementService
      .getAgreements(_: String)(_: UUID, _: UUID))
      .expects(bearerToken, eServiceId, consumerId)
      .once()
      .returns(Future.successful(result))

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

  def mockRiskAnalysisPdfCreation(): CallHandler3[String, RiskAnalysisForm, Int, Future[File]] = {
    val documentId = UUID.randomUUID()
    val tempFile   = File.createTempFile(UUID.randomUUID().toString, UUID.randomUUID().toString)
    tempFile.deleteOnExit()

    (() => mockUUIDSupplier.get).expects().returning(documentId).once()
    (mockPdfCreator
      .createDocument(_: String, _: PurposeManagement.RiskAnalysisForm, _: Int))
      .expects(*, *, *)
      .returning(Future.successful(tempFile))
      .once()
  }

  def mockVersionFirstActivation(
    purposeId: UUID,
    versionId: UUID,
    result: PurposeManagement.PurposeVersion
  ): CallHandler4[String, UUID, UUID, ActivatePurposeVersionPayload, Future[client.model.PurposeVersion]] = {
    mockRiskAnalysisPdfCreation()

    (() => mockDateTimeSupplier.get).expects().returning(SpecData.timestamp).once()

    (mockPurposeManagementService
      .activatePurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload))
      .expects(bearerToken, purposeId, versionId, *)
      .once()
      .returns(Future.successful(result))
  }

  def mockVersionLoadValidation(
    activatingPurpose: PurposeManagement.Purpose,
    existingPurposes: PurposeManagement.Purposes,
    descriptorId: UUID
  ): CallHandler3[String, UUID, UUID, Future[Seq[Agreement]]] = {
    val producerId = UUID.randomUUID()
    mockPurposesRetrieve(
      eServiceId = Some(activatingPurpose.eserviceId),
      consumerId = Some(activatingPurpose.consumerId),
      states = Seq(PurposeManagement.PurposeVersionState.ACTIVE),
      result = existingPurposes
    )

    mockAgreementsRetrieve(
      activatingPurpose.eserviceId,
      activatingPurpose.consumerId,
      Seq(
        SpecData.agreement.copy(
          consumerId = activatingPurpose.consumerId,
          eserviceId = activatingPurpose.eserviceId,
          descriptorId = descriptorId,
          producerId = producerId
        )
      )
    )
  }

  def mockVersionWaitForApproval(
    purposeId: UUID,
    versionId: UUID,
    stateChangeDetails: PurposeManagement.StateChangeDetails,
    result: PurposeManagement.PurposeVersion
  ): CallHandler4[String, UUID, UUID, StateChangeDetails, Future[client.model.PurposeVersion]] =
    (mockPurposeManagementService
      .waitForApprovalPurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails))
      .expects(bearerToken, purposeId, versionId, stateChangeDetails)
      .once()
      .returns(Future.successful(result))

  def mockVersionActivate(
    purposeId: UUID,
    versionId: UUID,
    result: PurposeManagement.PurposeVersion
  ): CallHandler4[String, UUID, UUID, ActivatePurposeVersionPayload, Future[client.model.PurposeVersion]] =
    (mockPurposeManagementService
      .activatePurposeVersion(_: String)(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload))
      .expects(bearerToken, purposeId, versionId, *)
      .once()
      .returns(Future.successful(result))

  def mockAssertUserConsumer(
    userId: UUID,
    consumerId: UUID,
    result: PartyManagement.Relationships
  ): CallHandler3[String, UUID, UUID, Future[Relationships]] =
    mockRelationshipsRetrieve(userId, consumerId, result)

  def mockAssertUserProducer(
    userId: UUID,
    consumerId: UUID,
    eService: CatalogManagement.EService,
    relationships: PartyManagement.Relationships
  ): CallHandler3[String, UUID, UUID, Future[Relationships]] = {
    mockAssertUserConsumer(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))
    mockEServiceRetrieve(eService.id, eService)
    mockRelationshipsRetrieve(userId, eService.producerId, relationships)
  }

  def mockClientStateUpdate(
    purposeId: UUID,
    state: AuthorizationManagement.ClientComponentState
  ): CallHandler3[String, UUID, AuthorizationManagement.ClientComponentState, Future[Unit]] =
    (mockAuthorizationManagementService
      .updateStateOnClients(_: String)(_: UUID, _: AuthorizationManagement.ClientComponentState))
      .expects(bearerToken, purposeId, state)
      .returning(Future.successful(()))
      .once()

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
