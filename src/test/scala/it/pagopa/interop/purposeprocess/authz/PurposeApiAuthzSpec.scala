package it.pagopa.interop.purposeprocess.authz

import it.pagopa.interop.commons.cqrs.model.ReadModelConfig
import it.pagopa.interop.commons.cqrs.service.{MongoDbReadModelService, ReadModelService}
import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiMarshallerImpl._
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiServiceImpl
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposemanagement.model.purpose.PersistentRiskAnalysisForm
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind
import it.pagopa.interop.purposeprocess.model.EServiceInfo
import it.pagopa.interop.purposeprocess.service._
import it.pagopa.interop.purposeprocess.util.FakeDependencies._
import it.pagopa.interop.purposeprocess.util.{AuthorizedRoutes, AuthzScalatestRouteTest}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File
import java.time.OffsetDateTime
import java.util.UUID
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import it.pagopa.interop.commons.riskanalysis.model.riskAnalysisTemplate.Language

class PurposeApiAuthzSpec extends AnyWordSpecLike with BeforeAndAfterAll with AuthzScalatestRouteTest {

  val fakeCatalogManagementService: CatalogManagementService             = new FakeCatalogManagementService()
  val fakePurposeManagementService: PurposeManagementService             = new FakePurposeManagementService()
  val fakeAgreementManagementService: AgreementManagementService         = new FakeAgreementManagementService()
  val fakeAuthorizationManagementService: AuthorizationManagementService = new FakeAuthorizationManagementService()
  val fakeTenantManagementService: TenantManagementService               = new FakeTenantManagementService()
  implicit val dummyReadModel: ReadModelService                          = new MongoDbReadModelService(
    ReadModelConfig(
      "mongodb://localhost/?socketTimeoutMS=1&serverSelectionTimeoutMS=1&connectTimeoutMS=1&&autoReconnect=false&keepAlive=false",
      "db"
    )
  )
  private val threadPool: ExecutorService                                = Executors.newSingleThreadExecutor()
  private val blockingEc: ExecutionContextExecutor = ExecutionContext.fromExecutorService(threadPool)
  val fakeFileManager: FileManager                 = FileManager.get(FileManager.File)(blockingEc)

  override def afterAll(): Unit = { threadPool.shutdown() }

  val service: PurposeApiServiceImpl =
    PurposeApiServiceImpl(
      fakeAgreementManagementService,
      fakeAuthorizationManagementService,
      fakeCatalogManagementService,
      fakePurposeManagementService,
      fakeTenantManagementService,
      fakeFileManager,
      pdfCreator = new PDFCreator {
        override def createDocument(
          template: String,
          riskAnalysisForm: PersistentRiskAnalysisForm,
          dailyCalls: Int,
          eServiceInfo: EServiceInfo,
          isFreeOfCharge: Boolean,
          freeOfChargeReason: Option[String],
          language: Language
        )(kind: PersistentTenantKind): Future[File] = Future.successful(File.createTempFile("full", "fake"))
      },
      uuidSupplier = new UUIDSupplier {
        override def get: UUID = UUID.randomUUID()
      },
      dateTimeSupplier = new OffsetDateTimeSupplier {
        override def get: OffsetDateTime = OffsetDateTime.now()
      }
    )

  "Purpose api operation authorization spec" should {
    "accept authorized roles for createPurpose" in {
      val endpoint = AuthorizedRoutes.endpoints("createPurpose")
      val fakeSeed = PurposeSeed(
        eserviceId = UUID.randomUUID(),
        consumerId = UUID.randomUUID(),
        title = "???",
        description = "???",
        riskAnalysisForm = None,
        isFreeOfCharge = false,
        dailyCalls = 100
      )
      validateAuthorization(endpoint, { implicit c: Seq[(String, String)] => service.createPurpose(fakeSeed) })
    }

    "accept authorized roles for createPurposeFromEService" in {
      val endpoint = AuthorizedRoutes.endpoints("createPurposeFromEService")
      val fakeSeed = EServicePurposeSeed(
        eServiceId = UUID.randomUUID(),
        consumerId = UUID.randomUUID(),
        riskAnalysisId = UUID.randomUUID(),
        title = "???",
        description = "???",
        isFreeOfCharge = false,
        dailyCalls = 100
      )
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.createPurposeFromEService(fakeSeed) }
      )
    }

    "accept authorized roles for clonePurpose" in {
      val endpoint = AuthorizedRoutes.endpoints("clonePurpose")
      validateAuthorization(endpoint, { implicit c: Seq[(String, String)] => service.clonePurpose("fakeSeed") })
    }

    "accept authorized roles for getPurpose" in {
      val endpoint = AuthorizedRoutes.endpoints("getPurpose")
      validateAuthorization(endpoint, { implicit c: Seq[(String, String)] => service.getPurpose("fakeSeed") })
    }

    "accept authorized roles for deletePurpose" in {
      val endpoint = AuthorizedRoutes.endpoints("deletePurpose")
      validateAuthorization(endpoint, { implicit c: Seq[(String, String)] => service.deletePurpose("fakeSeed") })
    }

    "accept authorized roles for createPurposeVersion" in {
      val endpoint = AuthorizedRoutes.endpoints("createPurposeVersion")
      val fakeSeed = PurposeVersionSeed(1)
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.createPurposeVersion("fakeSeed", fakeSeed) }
      )
    }

    "accept authorized roles for deletePurposeVersion" in {
      val endpoint = AuthorizedRoutes.endpoints("deletePurposeVersion")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.deletePurposeVersion("fakeSeed", "fake") }
      )
    }

    "accept authorized roles for activatePurposeVersion " in {
      val endpoint = AuthorizedRoutes.endpoints("activatePurposeVersion")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.activatePurposeVersion("fakeSeed", "fake") }
      )
    }

    "accept authorized roles for suspendPurposeVersion" in {
      val endpoint = AuthorizedRoutes.endpoints("suspendPurposeVersion")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] =>
          service.suspendPurposeVersion("fakeSeed", "fake")
        }
      )
    }

    "accept authorized roles for archivePurposeVersion" in {
      val endpoint = AuthorizedRoutes.endpoints("archivePurposeVersion")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] =>
          service.archivePurposeVersion("fake", "fake")
        }
      )
    }

    "accept authorized roles for getPurposes" in {
      val endpoint = AuthorizedRoutes.endpoints("getPurposes")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.getPurposes(None, "fake", "fake", "fake", "fake", false, 0, 0) }
      )
    }

    "accept authorized roles for updatePurpose" in {
      val endpoint    = AuthorizedRoutes.endpoints("updatePurpose")
      val fakeContent = PurposeUpdateContent("test", "Fake", false, None, None, 100)
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.updatePurpose("fake", fakeContent) }
      )
    }

    "accept authorized roles for updateReversePurpose" in {
      val endpoint    = AuthorizedRoutes.endpoints("updateReversePurpose")
      val fakeContent = ReversePurposeUpdateContent("test", "Fake", false, None, 100)
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.updateReversePurpose("fake", fakeContent) }
      )
    }
    "accept authorized roles for updateWaitingForApprovalPurposeVersion" in {
      val endpoint    = AuthorizedRoutes.endpoints("updateWaitingForApprovalPurposeVersion")
      val fakeContent = WaitingForApprovalPurposeVersionUpdateContent(OffsetDateTime.now())
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] =>
          service.updateWaitingForApprovalPurposeVersion("fakeSeed", "fake", fakeContent)
        }
      )
    }

    "accept authorized roles for getRiskAnalysisDocument" in {
      val endpoint = AuthorizedRoutes.endpoints("getRiskAnalysisDocument")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.getRiskAnalysisDocument("fake", "fake", "fake") }
      )
    }

    "accept authorized roles for retrieveLatestRiskAnalysisConfiguration" in {
      val endpoint = AuthorizedRoutes.endpoints("retrieveLatestRiskAnalysisConfiguration")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.retrieveLatestRiskAnalysisConfiguration(None) }
      )
    }

    "accept authorized roles for retrieveRiskAnalysisConfigurationByVersion" in {
      val endpoint = AuthorizedRoutes.endpoints("retrieveRiskAnalysisConfigurationByVersion")
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.retrieveRiskAnalysisConfigurationByVersion(None, "fake") }
      )
    }

  }
}
