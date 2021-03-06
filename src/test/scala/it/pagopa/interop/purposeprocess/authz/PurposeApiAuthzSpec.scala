package it.pagopa.interop.purposeprocess.authz

import it.pagopa.interop.commons.files.service.FileManager
import it.pagopa.interop.commons.utils.service.{OffsetDateTimeSupplier, UUIDSupplier}
import it.pagopa.interop.purposemanagement.client.model.RiskAnalysisForm
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiServiceImpl
import it.pagopa.interop.purposeprocess.api.impl.PurposeApiMarshallerImpl._
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.{EServiceInfo, Language}
import it.pagopa.interop.purposeprocess.model.{
  DraftPurposeVersionUpdateContent,
  PurposeSeed,
  PurposeUpdateContent,
  PurposeVersionSeed,
  WaitingForApprovalPurposeVersionUpdateContent
}
import it.pagopa.interop.purposeprocess.service._
import it.pagopa.interop.purposeprocess.util.{AuthorizedRoutes, AuthzScalatestRouteTest}
import it.pagopa.interop.purposeprocess.util.FakeDependencies._
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File
import java.time.OffsetDateTime
import java.util.UUID
import scala.concurrent.Future
import org.scalatest.BeforeAndAfterAll
import java.util.concurrent.{Executors, ExecutorService}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class PurposeApiAuthzSpec extends AnyWordSpecLike with BeforeAndAfterAll with AuthzScalatestRouteTest {

  val fakeCatalogManagementService: CatalogManagementService             = new FakeCatalogManagementService()
  val fakePartyManagementService: PartyManagementService                 = new FakePartyManagementService()
  val fakePurposeManagementService: PurposeManagementService             =
    new FakePurposeManagementService()
  val fakeAgreementManagementService: AgreementManagementService         = new FakeAgreementManagementService()
  val fakeAuthorizationManagementService: AuthorizationManagementService = new FakeAuthorizationManagementService()

  private val threadPool: ExecutorService          = Executors.newSingleThreadExecutor()
  private val blockingEc: ExecutionContextExecutor = ExecutionContext.fromExecutorService(threadPool)
  val fakeFileManager: FileManager                 = FileManager.get(FileManager.File)(blockingEc)

  override def afterAll(): Unit = { threadPool.shutdown() }

  val service: PurposeApiServiceImpl =
    PurposeApiServiceImpl(
      fakeAgreementManagementService,
      fakeAuthorizationManagementService,
      fakeCatalogManagementService,
      fakePartyManagementService,
      fakePurposeManagementService,
      fakeFileManager,
      pdfCreator = new PDFCreator {
        override def createDocument(
          template: String,
          riskAnalysisForm: RiskAnalysisForm,
          dailyCalls: Int,
          eServiceInfo: EServiceInfo,
          language: Language
        ): Future[File] = Future.successful(File.createTempFile("full", "fake"))
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
        riskAnalysisForm = None
      )
      validateAuthorization(endpoint, { implicit c: Seq[(String, String)] => service.createPurpose(fakeSeed) })
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
      validateAuthorization(endpoint, { implicit c: Seq[(String, String)] => service.getPurposes(None, None, "fake") })
    }
    "accept authorized roles for updatePurpose" in {
      val endpoint    = AuthorizedRoutes.endpoints("updatePurpose")
      val fakeContent = PurposeUpdateContent("test", "Fake", None)
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.updatePurpose("fake", fakeContent) }
      )
    }

    "accept authorized roles for updateDraftPurposeVersion" in {
      val endpoint    = AuthorizedRoutes.endpoints("updateDraftPurposeVersion")
      val fakeContent = DraftPurposeVersionUpdateContent(1)
      validateAuthorization(
        endpoint,
        { implicit c: Seq[(String, String)] => service.updateDraftPurposeVersion("fake", "fake", fakeContent) }
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

  }
}
