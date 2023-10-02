package it.pagopa.interop.purposeprocess

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.directives.FileInfo
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import com.nimbusds.jwt.JWTClaimsSet
import com.typesafe.config.{Config, ConfigFactory}
import it.pagopa.interop.agreementmanagement.model.agreement.{
  PersistentAgreement,
  PersistentAgreementState,
  Active => AgreementActive
}
import it.pagopa.interop.authorizationmanagement.client.{model => AuthorizationManagement}
import it.pagopa.interop.authorizationmanagement.model.client.PersistentClient
import it.pagopa.interop.catalogmanagement.model.CatalogItem
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposemanagement.client.{model => PurposeManagement}
import it.pagopa.interop.purposemanagement.model.purpose.{
  Active,
  PersistentPurpose,
  PersistentPurposeVersionState,
  PersistentRiskAnalysisForm
}
import it.pagopa.interop.purposeprocess.SpecData.timestamp
import it.pagopa.interop.purposeprocess.api.PurposeApiService
import it.pagopa.interop.purposeprocess.api.impl._
import it.pagopa.interop.purposeprocess.common.readmodel.PaginatedResult
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.PurposeNotFound
import it.pagopa.interop.purposeprocess.model.EServiceInfo
import it.pagopa.interop.commons.riskanalysis.model.riskAnalysisTemplate.Language
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposeprocess.service._
import it.pagopa.interop.tenantmanagement.model.tenant.{PersistentTenant, PersistentTenantKind}
import org.scalamock.scalatest.MockFactory
import spray.json._

import java.io.{ByteArrayOutputStream, File}
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

trait SpecHelper extends SprayJsonSupport with DefaultJsonProtocol with MockFactory {

  final val bearerToken: String = "token"

  val config: Config = ConfigFactory
    .parseResourcesAnySyntax("application-test")
    .resolve()

  implicit val mockReadModel: ReadModelService                           = mock[ReadModelService]
  val mockfileManager: FileManager                                       = mock[FileManager]
  val mockAgreementManagementService: AgreementManagementService         = mock[AgreementManagementService]
  val mockAuthorizationManagementService: AuthorizationManagementService = mock[AuthorizationManagementService]
  val mockPurposeManagementService: PurposeManagementService             = mock[PurposeManagementService]
  val mockCatalogManagementService: CatalogManagementService             = mock[CatalogManagementService]
  val mockTenantManagementService: TenantManagementService               = mock[TenantManagementService]

  val mockPdfCreator: PDFCreator                   = mock[PDFCreator]
  val mockUUIDSupplier: UUIDSupplier               = mock[UUIDSupplier]
  val mockDateTimeSupplier: OffsetDateTimeSupplier = mock[OffsetDateTimeSupplier]

  val service: PurposeApiService = PurposeApiServiceImpl(
    mockAgreementManagementService,
    mockAuthorizationManagementService,
    mockCatalogManagementService,
    mockPurposeManagementService,
    mockTenantManagementService,
    mockfileManager,
    mockPdfCreator,
    mockUUIDSupplier,
    mockDateTimeSupplier
  )(ExecutionContext.global, mockReadModel)

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

  def mockEServiceRetrieve(eServiceId: UUID, result: CatalogItem = SpecData.eService) =
    (mockCatalogManagementService
      .getEServiceById(_: UUID)(_: ExecutionContext, _: ReadModelService))
      .expects(eServiceId, *, *)
      .once()
      .returns(Future.successful(result.copy(id = eServiceId)))

  def mockOrganizationRetrieve(tenantId: UUID) = {
    (mockTenantManagementService
      .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
      .expects(tenantId, *, *)
      .once()
      .returns(Future.successful(SpecData.tenant.copy(id = tenantId)))
  }

  def mockPurposeRetrieve(purposeId: UUID, result: PersistentPurpose = SpecData.purpose) =
    (mockPurposeManagementService
      .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
      .expects(purposeId, *, *)
      .once()
      .returns(Future.successful(result.copy(id = purposeId)))

  def mockPurposeRetrieveError(purposeId: UUID) = (mockPurposeManagementService
    .getPurposeById(_: UUID)(_: ExecutionContext, _: ReadModelService))
    .expects(*, *, *)
    .once()
    .returns(Future.failed(PurposeNotFound(purposeId)))

  def mockPurposeDelete(purposeId: UUID)(implicit contexts: Seq[(String, String)]) =
    (mockPurposeManagementService
      .deletePurpose(_: UUID)(_: Seq[(String, String)]))
      .expects(purposeId, contexts)
      .once()
      .returns(Future.unit)

  def mockPurposeUpdate(
    purposeId: UUID,
    purposeUpdateContent: PurposeManagement.PurposeUpdateContent,
    result: PurposeManagement.Purpose
  )(implicit contexts: Seq[(String, String)]) =
    (mockPurposeManagementService
      .updatePurpose(_: UUID, _: PurposeManagement.PurposeUpdateContent)(_: Seq[(String, String)]))
      .expects(purposeId, purposeUpdateContent, contexts)
      .once()
      .returns(Future.successful(result))

  def mockPurposeVersionCreate(
    purposeId: UUID,
    seed: PurposeManagement.PurposeVersionSeed,
    result: PurposeManagement.PurposeVersion
  )(implicit contexts: Seq[(String, String)]) =
    (mockPurposeManagementService
      .createPurposeVersion(_: UUID, _: PurposeManagement.PurposeVersionSeed)(_: Seq[(String, String)]))
      .expects(purposeId, seed, contexts)
      .once()
      .returns(Future.successful(result))

  def mockPurposeVersionDelete(purposeId: UUID, versionId: UUID)(implicit contexts: Seq[(String, String)]) =
    (mockPurposeManagementService
      .deletePurposeVersion(_: UUID, _: UUID)(_: Seq[(String, String)]))
      .expects(purposeId, versionId, contexts)
      .once()
      .returns(Future.unit)

  def mockPurposesRetrieve(
    eServiceId: Option[UUID] = None,
    consumerId: Option[UUID] = None,
    states: Seq[PersistentPurposeVersionState] = Seq.empty,
    result: Seq[PersistentPurpose] = SpecData.purposes
  ) =
    (mockPurposeManagementService
      .getPurposes(_: Option[UUID], _: Option[UUID], _: Seq[PersistentPurposeVersionState])(
        _: ExecutionContext,
        _: ReadModelService
      ))
      .expects(eServiceId, consumerId, states, *, *)
      .once()
      .returns(Future.successful(result))

  def mockAgreementsRetrieve(
    eServiceId: UUID,
    consumerId: UUID,
    states: Seq[PersistentAgreementState],
    result: Seq[PersistentAgreement] = Seq(SpecData.agreement)
  ) =
    (mockAgreementManagementService
      .getAgreements(_: UUID, _: UUID, _: Seq[PersistentAgreementState])(_: ExecutionContext, _: ReadModelService))
      .expects(eServiceId, consumerId, states, *, *)
      .once()
      .returns(Future.successful(result))

  def mockListPurposesRetrieve(result: Seq[PersistentPurpose]) =
    (mockPurposeManagementService
      .listPurposes(
        _: UUID,
        _: Option[String],
        _: List[String],
        _: List[String],
        _: List[String],
        _: List[PersistentPurposeVersionState],
        _: Boolean,
        _: Int,
        _: Int,
        _: Boolean
      )(_: ExecutionContext, _: ReadModelService))
      .expects(*, *, *, *, *, *, *, *, *, *, *, *)
      .once()
      .returns(Future.successful(PaginatedResult(results = result, totalCount = result.size)))

  def mockTenantRetrieve(tenantId: UUID, result: PersistentTenant) =
    (mockTenantManagementService
      .getTenantById(_: UUID)(_: ExecutionContext, _: ReadModelService))
      .expects(tenantId, *, *)
      .once()
      .returns(Future.successful(result))

  def mockRiskAnalysisPdfCreation() = {
    val documentId = UUID.randomUUID()
    val tempFile   = File.createTempFile(UUID.randomUUID().toString, UUID.randomUUID().toString)
    tempFile.deleteOnExit()

    (() => mockUUIDSupplier.get()).expects().returning(documentId).once()
    (mockPdfCreator
      .createDocument(
        _: String,
        _: PersistentRiskAnalysisForm,
        _: Int,
        _: EServiceInfo,
        _: Boolean,
        _: Option[String],
        _: Language
      )(_: PersistentTenantKind))
      .expects(*, *, *, *, *, *, *, *)
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

    (() => mockDateTimeSupplier.get()).expects().returning(SpecData.timestamp).once()

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
    activatingPurpose: PersistentPurpose,
    existingPurposes: Seq[PersistentPurpose],
    descriptorId: UUID
  ) = {
    val producerId = UUID.randomUUID()
    mockPurposesRetrieve(
      eServiceId = Some(activatingPurpose.eserviceId),
      consumerId = Some(activatingPurpose.consumerId),
      states = Seq(Active),
      result = existingPurposes
    )
    mockPurposesRetrieve(
      eServiceId = Some(activatingPurpose.eserviceId),
      consumerId = None,
      states = Seq(Active),
      result = existingPurposes
    )
    mockAgreementsRetrieve(
      activatingPurpose.eserviceId,
      activatingPurpose.consumerId,
      Seq(AgreementActive),
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

  def mockVersionLoadValidationAgreementNotFound(
    activatingPurpose: PersistentPurpose,
    existingPurposes: Seq[PersistentPurpose]
  ) = {
    mockPurposesRetrieve(
      eServiceId = Some(activatingPurpose.eserviceId),
      consumerId = Some(activatingPurpose.consumerId),
      states = Seq(Active),
      result = existingPurposes
    )
    mockPurposesRetrieve(
      eServiceId = Some(activatingPurpose.eserviceId),
      consumerId = None,
      states = Seq(Active),
      result = existingPurposes
    )
    mockAgreementsRetrieve(activatingPurpose.eserviceId, activatingPurpose.consumerId, Seq(AgreementActive), Seq.empty)
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

  def mockWaitingForApprovalVersionCreate(purposeId: UUID, result: PurposeManagement.PurposeVersion)(implicit
    context: Seq[(String, String)]
  ) = {
    val createdVersionSeed = PurposeManagement.PurposeVersionSeed(result.dailyCalls, result.riskAnalysis)

    mockPurposeVersionCreate(
      purposeId,
      createdVersionSeed,
      result.copy(state = PurposeManagement.PurposeVersionState.DRAFT)
    )
    mockVersionWaitForApproval(
      purposeId,
      result.id,
      PurposeManagement.StateChangeDetails(PurposeManagement.ChangedBy.CONSUMER, timestamp),
      result.copy(state = PurposeManagement.PurposeVersionState.WAITING_FOR_APPROVAL)
    )
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

  def mockClientsRetrieve(purposeId: UUID, result: Seq[PersistentClient] = Seq(SpecData.client)): Unit =
    (mockAuthorizationManagementService
      .getClients(_: UUID)(_: ExecutionContext, _: ReadModelService))
      .expects(purposeId, *, *)
      .returning(Future.successful(result))
      .once(): Unit

  def mockPurposeFromClientRemoval(purposeId: UUID, clientId: UUID)(implicit contexts: Seq[(String, String)]) =
    (mockAuthorizationManagementService
      .removePurposeFromClient(_: UUID, _: UUID)(_: Seq[(String, String)]))
      .expects(purposeId, clientId, contexts)
      .returning(Future.unit)
      .once()

  def mockPurposeEnhancement(purpose: PurposeManagement.Purpose, isConsumer: Boolean): Unit = {
    val agreement = SpecData.agreement
    mockAgreementsRetrieve(purpose.eserviceId, purpose.consumerId, Nil, Seq(agreement))
    mockOrganizationRetrieve(agreement.producerId)
    mockOrganizationRetrieve(agreement.consumerId)
    if (isConsumer) mockClientsRetrieve(purpose.id) else ()
  }

  implicit def fromResponseUnmarshallerOldPurpose: FromEntityUnmarshaller[Purpose]                            =
    sprayJsonUnmarshaller[Purpose]
  implicit def fromResponseUnmarshallerPurposeVersion: FromEntityUnmarshaller[PurposeVersion]                 =
    sprayJsonUnmarshaller[PurposeVersion]
  implicit def fromResponseUnmarshallerPurposeVersionDocument: FromEntityUnmarshaller[PurposeVersionDocument] =
    sprayJsonUnmarshaller[PurposeVersionDocument]
  implicit def fromResponseUnmarshallerPurposes: FromEntityUnmarshaller[Purposes]                             =
    sprayJsonUnmarshaller[Purposes]
  implicit def fromRiskAnalysisFormConfigResponse: FromEntityUnmarshaller[RiskAnalysisFormConfigResponse]     =
    sprayJsonUnmarshaller[RiskAnalysisFormConfigResponse]
  implicit def fromResponseUnmarshallerProblem: FromEntityUnmarshaller[Problem]                               =
    sprayJsonUnmarshaller[Problem]

}
