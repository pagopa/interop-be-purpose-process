package it.pagopa.interop.purposeprocess.server.impl

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.SecurityDirectives
import com.atlassian.oai.validator.report.ValidationReport
import com.nimbusds.jose.proc.SecurityContext
import com.nimbusds.jwt.proc.DefaultJWTClaimsVerifier
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.cqrs.service.{MongoDbReadModelService, ReadModelService}
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.jwt.service.JWTReader
import it.pagopa.interop.commons.jwt.service.impl.{DefaultJWTReader, getClaimsVerifier}
import it.pagopa.interop.commons.jwt.{JWTConfiguration, KID, PublicKeysHolder, SerializedKey}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.errors.{Problem => CommonProblem}
import it.pagopa.interop.commons.utils.service._
import it.pagopa.interop.commons.utils.{AkkaUtils, OpenapiUtils}
import it.pagopa.interop.purposeprocess.api.impl.ResponseHandlers.serviceCode
import it.pagopa.interop.purposeprocess.api.impl.{
  HealthApiMarshallerImpl,
  HealthServiceApiImpl,
  PurposeApiMarshallerImpl,
  PurposeApiServiceImpl
}
import it.pagopa.interop.purposeprocess.api.{HealthApi, PurposeApi}
import it.pagopa.interop.tenantmanagement.client.api.{TenantApi => TenantManagementApi}
import it.pagopa.interop.tenantmanagement.client.invoker.{ApiInvoker => TenantManagementInvoker}
import it.pagopa.interop.attributeregistrymanagement.client.invoker.{ApiInvoker => AttributeRegistryManagementInvoker}
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.interop.purposeprocess.service._
import it.pagopa.interop.purposeprocess.service.impl._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

trait Dependencies {

  implicit val loggerTI: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog]("OAuth2JWTValidatorAsContexts")

  val uuidSupplier: UUIDSupplier                               = UUIDSupplier
  val dateTimeSupplier: OffsetDateTimeSupplier                 = OffsetDateTimeSupplier
  val riskAnalysisServiceSupplier: RiskAnalysisServiceSupplier = RiskAnalysisServiceSupplier

  val pdfCreator: PDFCreatorImpl.type = PDFCreatorImpl

  def getFileManager(blockingEc: ExecutionContextExecutor): FileManager =
    FileManager.get(ApplicationConfiguration.storageKind match {
      case "S3"   => FileManager.S3
      case "file" => FileManager.File
      case _      => throw new Exception("Incorrect File Manager")
    })(blockingEc)

  def jwtValidator(): Future[JWTReader] = JWTConfiguration.jwtReader
    .loadKeyset()
    .map(keyset =>
      new DefaultJWTReader with PublicKeysHolder {
        var publicKeyset: Map[KID, SerializedKey]                                        = keyset
        override protected val claimsVerifier: DefaultJWTClaimsVerifier[SecurityContext] =
          getClaimsVerifier(audience = ApplicationConfiguration.jwtAudience)
      }
    )
    .toFuture

  val validationExceptionToRoute: ValidationReport => Route = report => {
    val error =
      CommonProblem(StatusCodes.BadRequest, OpenapiUtils.errorFromRequestValidationReport(report), serviceCode, None)
    complete(error.status, error)
  }

  val healthApi: HealthApi = new HealthApi(
    new HealthServiceApiImpl(),
    HealthApiMarshallerImpl,
    SecurityDirectives.authenticateOAuth2("SecurityRealm", AkkaUtils.PassThroughAuthenticator),
    loggingEnabled = false
  )

  val readModelService: ReadModelService = new MongoDbReadModelService(ApplicationConfiguration.readModelConfig)

  def purposeApi(jwtReader: JWTReader, fileManager: FileManager, blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_],
    ec: ExecutionContext
  ): PurposeApi =
    new PurposeApi(
      PurposeApiServiceImpl(
        agreementManagement(blockingEc),
        authorizationManagement(blockingEc),
        catalogManagement(blockingEc),
        purposeManagement(blockingEc),
        tenantManagement(blockingEc),
        attributeRegistryManagement(blockingEc),
        readModelService,
        fileManager,
        pdfCreator,
        uuidSupplier,
        dateTimeSupplier,
        riskAnalysisServiceSupplier
      ),
      PurposeApiMarshallerImpl,
      jwtReader.OAuth2JWTValidatorAsContexts
    )

  private def attributeRegistryManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): AttributeRegistryManagementInvoker =
    AttributeRegistryManagementInvoker(blockingEc)(actorSystem.classicSystem)

  private final val attributeRegistryManagementApi: AttributeRegistryManagementApi = AttributeRegistryManagementApi(
    ApplicationConfiguration.attributeRegistryManagementURL
  )

  def attributeRegistryManagement(
    blockingEc: ExecutionContextExecutor
  )(implicit actorSystem: ActorSystem[_]): AttributeRegistryManagementService =
    AttributeRegistryManagementServiceImpl(
      attributeRegistryManagementInvoker(blockingEc),
      attributeRegistryManagementApi
    )

  private def agreementManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): AgreementManagementInvoker =
    AgreementManagementInvoker(blockingEc)(actorSystem.classicSystem)

  private final val agreementManagementApi: AgreementManagementApi                       = AgreementManagementApi(
    ApplicationConfiguration.agreementManagementURL
  )

  def agreementManagement(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): AgreementManagementService =
    AgreementManagementServiceImpl(agreementManagementInvoker(blockingEc), agreementManagementApi)

  private def authorizationManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): AuthorizationManagementInvoker =
    AuthorizationManagementInvoker(blockingEc)(actorSystem.classicSystem)
  private final val authorizationManagementPurposeApi: AuthorizationManagementPurposeApi =
    AuthorizationManagementPurposeApi(ApplicationConfiguration.authorizationManagementURL)
  private final val authorizationManagementClientApi: AuthorizationManagementClientApi   =
    AuthorizationManagementClientApi(ApplicationConfiguration.authorizationManagementURL)

  def authorizationManagement(
    blockingEc: ExecutionContextExecutor
  )(implicit actorSystem: ActorSystem[_], ec: ExecutionContext): AuthorizationManagementService =
    AuthorizationManagementServiceImpl(
      authorizationManagementInvoker(blockingEc),
      authorizationManagementPurposeApi,
      authorizationManagementClientApi
    )

  private def catalogManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): CatalogManagementInvoker =
    CatalogManagementInvoker(blockingEc)(actorSystem.classicSystem)
  private final val catalogManagementApi: CatalogManagementApi                           = CatalogManagementApi(
    ApplicationConfiguration.catalogManagementURL
  )

  def catalogManagement(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): CatalogManagementService = CatalogManagementServiceImpl(catalogManagementInvoker(blockingEc), catalogManagementApi)

  private def purposeManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): PurposeManagementInvoker =
    PurposeManagementInvoker(blockingEc)(actorSystem.classicSystem)
  private final val purposeManagementApi: PurposeManagementApi                           = PurposeManagementApi(
    ApplicationConfiguration.purposeManagementURL
  )

  def purposeManagement(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): PurposeManagementService =
    PurposeManagementServiceImpl(purposeManagementInvoker(blockingEc), purposeManagementApi)(blockingEc)

  private def tenantManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): TenantManagementInvoker =
    TenantManagementInvoker(blockingEc)(actorSystem.classicSystem)

  private final val tenantManagementApi: TenantManagementApi = TenantManagementApi(
    ApplicationConfiguration.tenantManagementURL
  )

  def tenantManagement(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): TenantManagementService =
    TenantManagementServiceImpl(tenantManagementInvoker(blockingEc), tenantManagementApi)(blockingEc)
}
