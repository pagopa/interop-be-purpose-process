package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import com.nimbusds.jwt.JWTClaimsSet
import com.typesafe.config.{Config, ConfigFactory}
import it.pagopa.interop.agreementmanagement.client.{model => AgreementManagement}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagement}
import it.pagopa.interop.catalogmanagement.client.{model => CatalogManagement}
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.selfcare.partymanagement.client.{model => PartyManagement}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.interop.purposeprocess.api.PurposeApiService
import it.pagopa.interop.purposeprocess.api.impl._
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.{EServiceInfo, Language}
import it.pagopa.interop.purposeprocess.model.{Problem, Purpose, PurposeVersion, Purposes}
import it.pagopa.interop.purposeprocess.service._
import org.scalamock.scalatest.MockFactory
import spray.json._

import java.io.File
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}
import akka.http.scaladsl.server.directives.FileInfo
import java.io.ByteArrayOutputStream
import it.pagopa.interop.purposemanagement.client.invoker.{ApiError => PurposeApiError}
import it.pagopa.interop.purposemanagement.client.model.{Problem => PurposeProblem}

trait SpecHelper extends SprayJsonSupport with DefaultJsonProtocol with MockFactory {

  final val bearerToken: String = "token"

  val config: Config = ConfigFactory
    .parseResourcesAnySyntax("application-test")
    .resolve()

  val mockfileManager: FileManager                                       = mock[FileManager]
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
      mockfileManager,
      mockPdfCreator,
      mockUUIDSupplier,
      mockDateTimeSupplier
    )(ExecutionContext.global)

  def mockFileManagerStore(storageFilePath: String) = (
    mockfileManager.store(_: String, _: String)(_: String, _: (FileInfo, File))
  ).expects(*, *, *, *).once().returns(Future.successful(storageFilePath))

  def mockFileManagerGet(filePath: String)(returns: Array[Byte]) =
    (mockfileManager.get(_: String)(_: String)).expects(*, filePath).once().returns {
      val b = new ByteArrayOutputStream
      b.writeBytes(returns)
      Future.successful(b)
    }

  def mockSubject(uuid: String): Try[JWTClaimsSet] = Success(new JWTClaimsSet.Builder().subject(uuid).build())

  def mockEServiceRetrieve(eServiceId: UUID, result: CatalogManagement.EService = SpecData.eService)(implicit
    contexts: Seq[(String, String)]
  ) =
    (mockCatalogManagementService
      .getEServiceById(_: UUID)(_: Seq[(String, String)]))
      .expects(eServiceId, contexts)
      .once()
      .returns(Future.successful(result.copy(id = eServiceId)))

  def mockOrganizationRetrieve(institutionId: UUID) =
    (mockPartyManagementService
      .getInstitutionById(_: UUID)(_: Seq[(String, String)], _: ExecutionContext))
      .expects(institutionId, *, *)
      .once()
      .returns(Future.successful(SpecData.institution.copy(id = institutionId)))

  def mockPurposeRetrieve(purposeId: UUID, result: PurposeManagement.Purpose = SpecData.purpose)(implicit
    contexts: Seq[(String, String)]
  ) =
    (mockPurposeManagementService
      .getPurpose(_: UUID)(_: Seq[(String, String)]))
      .expects(purposeId, contexts)
      .once()
      .returns(Future.successful(result.copy(id = purposeId)))

  def mockPurposeRetrieveError(problem: PurposeProblem) = (mockPurposeManagementService
    .getPurpose(_: UUID)(_: Seq[(String, String)]))
    .expects(*, *)
    .once()
    .returns(Future.failed(PurposeApiError[String](problem.status, "Some error", Some(problem.toJson.prettyPrint))))

  def mockPurposeDelete(purposeId: UUID)(implicit contexts: Seq[(String, String)]) =
    (mockPurposeManagementService
      .deletePurpose(_: UUID)(_: Seq[(String, String)]))
      .expects(purposeId, contexts)
      .once()
      .returns(Future.unit)

  def mockPurposeVersionDelete(purposeId: UUID, versionId: UUID)(implicit contexts: Seq[(String, String)]) =
    (mockPurposeManagementService
      .deletePurposeVersion(_: UUID, _: UUID)(_: Seq[(String, String)]))
      .expects(purposeId, versionId, contexts)
      .once()
      .returns(Future.unit)

  def mockPurposesRetrieve(
    eServiceId: Option[UUID] = None,
    consumerId: Option[UUID] = None,
    states: Seq[PurposeManagement.PurposeVersionState] = Seq.empty,
    result: PurposeManagement.Purposes = SpecData.purposes
  )(implicit contexts: Seq[(String, String)]) =
    (mockPurposeManagementService
      .getPurposes(_: Option[UUID], _: Option[UUID], _: Seq[PurposeManagement.PurposeVersionState])(
        _: Seq[(String, String)]
      ))
      .expects(eServiceId, consumerId, states, contexts)
      .once()
      .returns(Future.successful(result))

  def mockAgreementsRetrieve(
    eServiceId: UUID,
    consumerId: UUID,
    result: Seq[AgreementManagement.Agreement] = Seq(SpecData.agreement)
  )(implicit contexts: Seq[(String, String)]) =
    (mockAgreementManagementService
      .getAgreements(_: UUID, _: UUID)(_: Seq[(String, String)]))
      .expects(eServiceId, consumerId, contexts)
      .once()
      .returns(Future.successful(result))

  def mockRelationshipsRetrieve(
    from: UUID,
    to: UUID,
    result: PartyManagement.Relationships = SpecData.relationships()
  ) =
    (mockPartyManagementService
      .getActiveRelationships(_: UUID, _: UUID)(_: Seq[(String, String)], _: ExecutionContext))
      .expects(from, to, *, *)
      .once()
      .returns(Future.successful(result))

  def mockRiskAnalysisPdfCreation() = {
    val documentId = UUID.randomUUID()
    val tempFile   = File.createTempFile(UUID.randomUUID().toString, UUID.randomUUID().toString)
    tempFile.deleteOnExit()

    (() => mockUUIDSupplier.get).expects().returning(documentId).once()
    (mockPdfCreator
      .createDocument(_: String, _: PurposeManagement.RiskAnalysisForm, _: Int, _: EServiceInfo, _: Language))
      .expects(*, *, *, *, *)
      .returning(Future.successful(tempFile))
      .once()
  }

  def mockVersionFirstActivation(
    purposeId: UUID,
    versionId: UUID,
    producerId: UUID,
    consumerId: UUID,
    result: PurposeManagement.PurposeVersion
  ) = {
    mockRiskAnalysisPdfCreation()

    (() => mockDateTimeSupplier.get).expects().returning(SpecData.timestamp).once()

    mockOrganizationRetrieve(producerId)
    mockOrganizationRetrieve(consumerId)

    (mockPurposeManagementService
      .activatePurposeVersion(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload)(
        _: Seq[(String, String)]
      ))
      .expects(purposeId, versionId, *, *)
      .once()
      .returns(Future.successful(result))
  }

  def mockVersionLoadValidation(
    activatingPurpose: PurposeManagement.Purpose,
    existingPurposes: PurposeManagement.Purposes,
    descriptorId: UUID
  )(implicit contexts: Seq[(String, String)]) = {
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
  )(implicit context: Seq[(String, String)]) =
    (mockPurposeManagementService
      .waitForApprovalPurposeVersion(_: UUID, _: UUID, _: PurposeManagement.StateChangeDetails)(
        _: Seq[(String, String)]
      ))
      .expects(purposeId, versionId, stateChangeDetails, context)
      .once()
      .returns(Future.successful(result))

  def mockVersionActivate(purposeId: UUID, versionId: UUID, result: PurposeManagement.PurposeVersion)(implicit
    context: Seq[(String, String)]
  ) =
    (mockPurposeManagementService
      .activatePurposeVersion(_: UUID, _: UUID, _: PurposeManagement.ActivatePurposeVersionPayload)(
        _: Seq[(String, String)]
      ))
      .expects(purposeId, versionId, *, context)
      .once()
      .returns(Future.successful(result))

  def mockAssertUserConsumer(userId: UUID, consumerId: UUID, result: PartyManagement.Relationships) =
    mockRelationshipsRetrieve(userId, consumerId, result)

  def mockAssertUserProducerIfNotConsumer(
    userId: UUID,
    consumerId: UUID,
    eService: CatalogManagement.EService,
    relationships: PartyManagement.Relationships
  )(implicit contexts: Seq[(String, String)]) = {
    mockAssertUserConsumer(userId, consumerId, SpecData.relationships().copy(items = Seq.empty))
    mockEServiceRetrieve(eService.id, eService)
    mockRelationshipsRetrieve(userId, eService.producerId, relationships)
  }

  def mockAssertUserProducer(
    userId: UUID,
    eService: CatalogManagement.EService,
    relationships: PartyManagement.Relationships
  )(implicit contexts: Seq[(String, String)]) = {
    mockEServiceRetrieve(eService.id, eService)
    mockRelationshipsRetrieve(userId, eService.producerId, relationships)
  }

  def mockClientStateUpdate(purposeId: UUID, versionId: UUID, state: AuthorizationManagement.ClientComponentState)(
    implicit contexts: Seq[(String, String)]
  ) =
    (mockAuthorizationManagementService
      .updateStateOnClients(_: UUID, _: UUID, _: AuthorizationManagement.ClientComponentState)(
        _: Seq[(String, String)]
      ))
      .expects(purposeId, versionId, state, contexts)
      .returning(Future.successful(()))
      .once()

  def mockClientsRetrieve(purposeId: Option[UUID], result: Seq[AuthorizationManagement.Client] = Seq(SpecData.client))(
    implicit contexts: Seq[(String, String)]
  ) =
    (mockAuthorizationManagementService
      .getClients(_: Option[UUID])(_: Seq[(String, String)]))
      .expects(purposeId, contexts)
      .returning(Future.successful(result))
      .once()

  def mockPurposeFromClientRemoval(purposeId: UUID, clientId: UUID)(implicit contexts: Seq[(String, String)]) =
    (mockAuthorizationManagementService
      .removePurposeFromClient(_: UUID, _: UUID)(_: Seq[(String, String)]))
      .expects(purposeId, clientId, contexts)
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
