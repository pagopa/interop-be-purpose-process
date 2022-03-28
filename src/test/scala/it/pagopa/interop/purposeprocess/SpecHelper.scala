package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import com.nimbusds.jwt.JWTClaimsSet
import com.typesafe.config.{Config, ConfigFactory}
import it.pagopa.interop.agreementmanagement.client.model.Agreement
import it.pagopa.interop.agreementmanagement.client.{model => AgreementManagement}
import it.pagopa.interop.authorizationmanagement.client.model.Client
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagement}
import it.pagopa.interop.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.partymanagement.client.model.Relationships
import it.pagopa.interop.partymanagement.client.{model => PartyManagement}
import it.pagopa.interop.purposemanagement.client
import it.pagopa.interop.purposemanagement.client.model.{
  ActivatePurposeVersionPayload,
  PurposeVersionState,
  RiskAnalysisForm,
  StateChangeDetails
}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.interop.purposeprocess.api.PurposeApiService
import it.pagopa.interop.purposeprocess.api.impl._
import it.pagopa.interop.purposeprocess.model.{Problem, Purpose, PurposeVersion, Purposes}
import it.pagopa.interop.purposeprocess.service._
import org.scalamock.handlers.{CallHandler2, CallHandler3, CallHandler4}
import org.scalamock.scalatest.MockFactory
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.io.File
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

trait SpecHelper extends SprayJsonSupport with DefaultJsonProtocol with MockFactory {

  final val bearerToken: String = "token"

  val config: Config           = ConfigFactory
    .parseResourcesAnySyntax("application-test")
    .resolve()
  val fileManagerType: String  = config.getString("interop-commons.storage.type")
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

  def mockEServiceRetrieve(eServiceId: UUID, result: CatalogManagement.EService = SpecData.eService)(implicit
    contexts: Seq[(String, String)]
  ): CallHandler2[Seq[(String, String)], UUID, Future[CatalogManagement.EService]] =
    (mockCatalogManagementService
      .getEServiceById(_: Seq[(String, String)])(_: UUID))
      .expects(contexts, eServiceId)
      .once()
      .returns(Future.successful(result.copy(id = eServiceId)))

  def mockOrganizationRetrieve(organizationId: UUID): CallHandler2[String, UUID, Future[PartyManagement.Organization]] =
    (mockPartyManagementService
      .getOrganizationById(_: String)(_: UUID))
      .expects(bearerToken, organizationId)
      .once()
      .returns(Future.successful(SpecData.organization.copy(id = organizationId)))

  def mockPurposeRetrieve(purposeId: UUID, result: PurposeManagement.Purpose = SpecData.purpose)(implicit
    contexts: Seq[(String, String)]
  ): CallHandler2[Seq[(String, String)], UUID, Future[ManagementPurpose]] =
    (mockPurposeManagementService
      .getPurpose(_: Seq[(String, String)])(_: UUID))
      .expects(contexts, purposeId)
      .once()
      .returns(Future.successful(result.copy(id = purposeId)))

  def mockPurposeDelete(
    purposeId: UUID
  )(implicit contexts: Seq[(String, String)]): CallHandler2[Seq[(String, String)], UUID, Future[Unit]] =
    (mockPurposeManagementService
      .deletePurpose(_: Seq[(String, String)])(_: UUID))
      .expects(contexts, purposeId)
      .once()
      .returns(Future.unit)

  def mockPurposeVersionDelete(purposeId: UUID, versionId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): CallHandler3[Seq[(String, String)], UUID, UUID, Future[Unit]] =
    (mockPurposeManagementService
      .deletePurposeVersion(_: Seq[(String, String)])(_: UUID, _: UUID))
      .expects(contexts, purposeId, versionId)
      .once()
      .returns(Future.unit)

  def mockPurposesRetrieve(
    eServiceId: Option[UUID] = None,
    consumerId: Option[UUID] = None,
    states: Seq[PurposeManagement.PurposeVersionState] = Seq.empty,
    result: PurposeManagement.Purposes = SpecData.purposes
  )(implicit contexts: Seq[(String, String)]): CallHandler4[Seq[(String, String)], Option[UUID], Option[UUID], Seq[
    PurposeVersionState
  ], Future[client.model.Purposes]] =
    (mockPurposeManagementService
      .getPurposes(_: Seq[(String, String)])(
        _: Option[UUID],
        _: Option[UUID],
        _: Seq[PurposeManagement.PurposeVersionState]
      ))
      .expects(contexts, eServiceId, consumerId, states)
      .once()
      .returns(Future.successful(result))

  def mockAgreementsRetrieve(
    eServiceId: UUID,
    consumerId: UUID,
    result: Seq[AgreementManagement.Agreement] = Seq(SpecData.agreement)
  )(implicit contexts: Seq[(String, String)]): CallHandler3[Seq[(String, String)], UUID, UUID, Future[Seq[Agreement]]] =
    (mockAgreementManagementService
      .getAgreements(_: Seq[(String, String)])(_: UUID, _: UUID))
      .expects(contexts, eServiceId, consumerId)
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

  def mockVersionFirstActivation(purposeId: UUID, versionId: UUID, result: PurposeManagement.PurposeVersion)(implicit
    contexts: Seq[(String, String)]
  ): CallHandler4[Seq[(String, String)], UUID, UUID, ActivatePurposeVersionPayload, Future[
    client.model.PurposeVersion
  ]] = {
    mockRiskAnalysisPdfCreation()

    (() => mockDateTimeSupplier.get).expects().returning(SpecData.timestamp).once()

    (mockPurposeManagementService
      .activatePurposeVersion(_: Seq[(String, String)])(
        _: UUID,
        _: UUID,
        _: PurposeManagement.ActivatePurposeVersionPayload
      ))
      .expects(contexts, purposeId, versionId, *)
      .once()
      .returns(Future.successful(result))
  }

  def mockVersionLoadValidation(
    activatingPurpose: PurposeManagement.Purpose,
    existingPurposes: PurposeManagement.Purposes,
    descriptorId: UUID
  )(implicit
    contexts: Seq[(String, String)]
  ): CallHandler3[Seq[(String, String)], UUID, UUID, Future[Seq[Agreement]]] = {
    val producerId = UUID.randomUUID()
    mockPurposesRetrieve(
      eServiceId = Some(activatingPurpose.eserviceId),
      consumerId = Some(activatingPurpose.consumerId),
      states = Seq(PurposeManagement.PurposeVersionState.ACTIVE),
      result = existingPurposes
    )
    mockPurposesRetrieve(
      eServiceId = Some(activatingPurpose.eserviceId),
      consumerId = None,
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
  )(implicit
    context: Seq[(String, String)]
  ): CallHandler4[Seq[(String, String)], UUID, UUID, StateChangeDetails, Future[client.model.PurposeVersion]] =
    (mockPurposeManagementService
      .waitForApprovalPurposeVersion(_: Seq[(String, String)])(
        _: UUID,
        _: UUID,
        _: PurposeManagement.StateChangeDetails
      ))
      .expects(context, purposeId, versionId, stateChangeDetails)
      .once()
      .returns(Future.successful(result))

  def mockVersionActivate(purposeId: UUID, versionId: UUID, result: PurposeManagement.PurposeVersion)(implicit
    context: Seq[(String, String)]
  ): CallHandler4[Seq[(String, String)], UUID, UUID, ActivatePurposeVersionPayload, Future[
    client.model.PurposeVersion
  ]] =
    (mockPurposeManagementService
      .activatePurposeVersion(_: Seq[(String, String)])(
        _: UUID,
        _: UUID,
        _: PurposeManagement.ActivatePurposeVersionPayload
      ))
      .expects(context, purposeId, versionId, *)
      .once()
      .returns(Future.successful(result))

  def mockAssertUserConsumer(
    userId: UUID,
    consumerId: UUID,
    result: PartyManagement.Relationships
  ): CallHandler3[String, UUID, UUID, Future[Relationships]] =
    mockRelationshipsRetrieve(userId, consumerId, result)

  def mockAssertUserProducerIfNotConsumer(
    userId: UUID,
    consumerId: UUID,
    eService: CatalogManagement.EService,
    relationships: PartyManagement.Relationships
  )(implicit contexts: Seq[(String, String)]): CallHandler3[String, UUID, UUID, Future[Relationships]] = {
    mockAssertUserConsumer(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))
    mockEServiceRetrieve(eService.id, eService)
    mockRelationshipsRetrieve(userId, eService.producerId, relationships)
  }

  def mockAssertUserProducer(
    userId: UUID,
    eService: CatalogManagement.EService,
    relationships: PartyManagement.Relationships
  )(implicit contexts: Seq[(String, String)]): CallHandler3[String, UUID, UUID, Future[Relationships]] = {
    mockEServiceRetrieve(eService.id, eService)
    mockRelationshipsRetrieve(userId, eService.producerId, relationships)
  }

  def mockClientStateUpdate(purposeId: UUID, state: AuthorizationManagement.ClientComponentState)(implicit
    contexts: Seq[(String, String)]
  ): CallHandler3[Seq[(String, String)], UUID, AuthorizationManagement.ClientComponentState, Future[Unit]] =
    (mockAuthorizationManagementService
      .updateStateOnClients(_: Seq[(String, String)])(_: UUID, _: AuthorizationManagement.ClientComponentState))
      .expects(contexts, purposeId, state)
      .returning(Future.successful(()))
      .once()

  def mockClientsRetrieve(purposeId: Option[UUID], result: Seq[AuthorizationManagement.Client] = Seq(SpecData.client))(
    implicit contexts: Seq[(String, String)]
  ): CallHandler2[Seq[(String, String)], Option[UUID], Future[Seq[Client]]] =
    (mockAuthorizationManagementService
      .getClients(_: Seq[(String, String)])(_: Option[UUID]))
      .expects(contexts, purposeId)
      .returning(Future.successful(result))
      .once()

  def mockPurposeFromClientRemoval(purposeId: UUID, clientId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): CallHandler3[Seq[(String, String)], UUID, UUID, Future[Unit]] =
    (mockAuthorizationManagementService
      .removePurposeFromClient(_: Seq[(String, String)])(_: UUID, _: UUID))
      .expects(contexts, purposeId, clientId)
      .returning(Future.unit)
      .once()

  def mockPurposeEnhancement(
    purpose: PurposeManagement.Purpose,
    isConsumer: Boolean,
    eService: Option[CatalogManagement.EService] = None
  )(implicit contexts: Seq[(String, String)]): Unit = {
    val agreement      = SpecData.agreement
    val descriptor     = SpecData.descriptor.copy(id = agreement.descriptorId)
    val actualEService = eService.getOrElse(SpecData.eService).copy(descriptors = Seq(descriptor))
    mockAgreementsRetrieve(purpose.eserviceId, purpose.consumerId, Seq(agreement))
    mockEServiceRetrieve(purpose.eserviceId, actualEService)
    mockOrganizationRetrieve(actualEService.producerId)
    if (isConsumer)
      mockClientsRetrieve(Some(purpose.id))

    ()
  }

  implicit def fromResponseUnmarshallerPurpose: FromEntityUnmarshaller[Purpose]               =
    sprayJsonUnmarshaller[Purpose]
  implicit def fromResponseUnmarshallerPurposeVersion: FromEntityUnmarshaller[PurposeVersion] =
    sprayJsonUnmarshaller[PurposeVersion]
  implicit def fromResponseUnmarshallerPurposes: FromEntityUnmarshaller[Purposes]             =
    sprayJsonUnmarshaller[Purposes]
  implicit def fromResponseUnmarshallerProblem: FromEntityUnmarshaller[Problem]               =
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
