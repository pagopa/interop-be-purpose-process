package it.pagopa.interop.purposeprocess.server.impl

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.SecurityDirectives
import com.atlassian.oai.validator.report.ValidationReport
import com.nimbusds.jose.proc.SecurityContext
import com.nimbusds.jwt.proc.DefaultJWTClaimsVerifier
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.jwt.service.JWTReader
import it.pagopa.interop.commons.jwt.service.impl.{DefaultJWTReader, getClaimsVerifier}
import it.pagopa.interop.commons.jwt.{JWTConfiguration, KID, PublicKeysHolder, SerializedKey}
import it.pagopa.interop.commons.utils.{AkkaUtils, OpenapiUtils}
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.commons.utils.errors.GenericComponentErrors
import it.pagopa.interop.commons.utils.service._
import it.pagopa.interop.commons.utils.service.impl._
import it.pagopa.interop.purposeprocess.api.impl.{
  HealthApiMarshallerImpl,
  HealthServiceApiImpl,
  PurposeApiMarshallerImpl,
  PurposeApiServiceImpl,
  problemOf
}
import it.pagopa.interop.purposeprocess.api.{HealthApi, PurposeApi}
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.interop.purposeprocess.service.impl._
import it.pagopa.interop.purposeprocess.service._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContextExecutor

trait Dependencies {

  implicit val partyManagementApiKeyValue: PartyManagementApiKeyValue = PartyManagementApiKeyValue()

  val uuidSupplier: UUIDSupplier               = new UUIDSupplierImpl()
  val dateTimeSupplier: OffsetDateTimeSupplier = OffsetDateTimeSupplierImpl

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
      problemOf(
        StatusCodes.BadRequest,
        GenericComponentErrors.ValidationRequestError(OpenapiUtils.errorFromRequestValidationReport(report))
      )
    complete(error.status, error)(HealthApiMarshallerImpl.toEntityMarshallerProblem)
  }

  val healthApi: HealthApi = new HealthApi(
    new HealthServiceApiImpl(),
    HealthApiMarshallerImpl,
    SecurityDirectives.authenticateOAuth2("SecurityRealm", AkkaUtils.PassThroughAuthenticator),
    loggingEnabled = false
  )

  def purposeApi(jwtReader: JWTReader, fileManager: FileManager, blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_],
    ec: ExecutionContext
  ): PurposeApi =
    new PurposeApi(
      PurposeApiServiceImpl(
        agreementManagement(blockingEc),
        authorizationManagement(blockingEc),
        catalogManagement(blockingEc),
        partyManagement(),
        purposeManagement(blockingEc),
        fileManager,
        pdfCreator,
        uuidSupplier,
        dateTimeSupplier
      ),
      PurposeApiMarshallerImpl,
      jwtReader.OAuth2JWTValidatorAsContexts
    )

  private def agreementManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): AgreementManagementInvoker =
    AgreementManagementInvoker(blockingEc)(actorSystem.classicSystem)

  private final val agreementManagementApi: AgreementManagementApi                       = AgreementManagementApi(
    ApplicationConfiguration.agreementManagementURL
  )

  def agreementManagement(
    blockingEc: ExecutionContextExecutor
  )(implicit actorSystem: ActorSystem[_], ec: ExecutionContext): AgreementManagementService =
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

  def catalogManagement(
    blockingEc: ExecutionContextExecutor
  )(implicit actorSystem: ActorSystem[_], ec: ExecutionContext): CatalogManagementService =
    CatalogManagementServiceImpl(catalogManagementInvoker(blockingEc), catalogManagementApi)

  private def partyManagementInvoker()(implicit actorSystem: ActorSystem[_]): PartyManagementInvoker =
    PartyManagementInvoker()(actorSystem.classicSystem)
  private final val partyManagementApi: PartyManagementApi = PartyManagementApi(
    ApplicationConfiguration.partyManagementURL
  )

  def partyManagement()(implicit actorSystem: ActorSystem[_]): PartyManagementService =
    PartyManagementServiceImpl(partyManagementInvoker(), partyManagementApi)

  private def purposeManagementInvoker(blockingEc: ExecutionContextExecutor)(implicit
    actorSystem: ActorSystem[_]
  ): PurposeManagementInvoker =
    PurposeManagementInvoker(blockingEc)(actorSystem.classicSystem)
  private final val purposeManagementApi: PurposeManagementApi                        = PurposeManagementApi(
    ApplicationConfiguration.purposeManagementURL
  )

  def purposeManagement(
    blockingEc: ExecutionContextExecutor
  )(implicit actorSystem: ActorSystem[_], ec: ExecutionContext): PurposeManagementService =
    PurposeManagementServiceImpl(purposeManagementInvoker(blockingEc), purposeManagementApi)

}
