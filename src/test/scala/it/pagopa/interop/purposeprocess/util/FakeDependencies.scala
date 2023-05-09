package it.pagopa.interop.purposeprocess.util

import cats.syntax.all._
import it.pagopa.interop.agreementmanagement.client.model.{Agreement, AgreementState}
import it.pagopa.interop.authorizationmanagement.client.model._
import it.pagopa.interop.catalogmanagement.client.model.{Attributes, EService, EServiceTechnology}
import it.pagopa.interop.commons.utils.service.OffsetDateTimeSupplier
import it.pagopa.interop.purposemanagement.client.model
import it.pagopa.interop.purposemanagement.client.model._
import it.pagopa.interop.purposeprocess.service._
import it.pagopa.interop.tenantmanagement.client.model.{ExternalId, Tenant, TenantKind}

import java.time.OffsetDateTime
import java.util.UUID
import scala.concurrent.Future

/**
 * Holds fake implementation of dependencies for tests not requiring neither mocks or stubs
 */
object FakeDependencies {

  class FakePurposeManagementService extends PurposeManagementService {
    override def createPurpose(
      seed: model.PurposeSeed
    )(implicit contexts: Seq[(String, String)]): Future[model.Purpose] = Future.successful(
      model.Purpose(
        id = UUID.randomUUID(),
        eserviceId = UUID.randomUUID(),
        consumerId = InvokerUUID.id,
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "???",
        description = "???",
        riskAnalysisForm = None,
        createdAt = OffsetDateTime.now(),
        updatedAt = None
      )
    )

    override def createPurposeVersion(purposeId: UUID, seed: PurposeVersionSeed)(implicit
      contexts: Seq[(String, String)]
    ): Future[PurposeVersion] = Future.successful(
      PurposeVersion(
        id = UUID.randomUUID(),
        state = PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def updatePurpose(purposeId: UUID, purposeUpdateContent: PurposeUpdateContent)(implicit
      contexts: Seq[(String, String)]
    ): Future[model.Purpose] = Future.successful(
      model.Purpose(
        id = UUID.randomUUID(),
        eserviceId = UUID.randomUUID(),
        consumerId = UUID.randomUUID(),
        versions = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        title = "???",
        description = "???",
        riskAnalysisForm = None,
        createdAt = OffsetDateTime.now(),
        updatedAt = None
      )
    )

    override def getPurpose(id: UUID)(implicit contexts: Seq[(String, String)]): Future[model.Purpose] =
      Future.successful(
        model.Purpose(
          id = UUID.randomUUID(),
          eserviceId = UUID.randomUUID(),
          consumerId = UUID.randomUUID(),
          versions = Seq.empty,
          suspendedByConsumer = None,
          suspendedByProducer = None,
          title = "???",
          description = "???",
          riskAnalysisForm = None,
          createdAt = OffsetDateTime.now(),
          updatedAt = None
        )
      )

    override def getPurposes(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState])(
      implicit contexts: Seq[(String, String)]
    ): Future[Purposes] = Future.successful(Purposes(Seq.empty))

    override def activatePurposeVersion(purposeId: UUID, versionId: UUID, payload: ActivatePurposeVersionPayload)(
      implicit contexts: Seq[(String, String)]
    ): Future[PurposeVersion] = Future.successful(
      PurposeVersion(
        id = UUID.randomUUID(),
        state = PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def suspendPurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(
      implicit contexts: Seq[(String, String)]
    ): Future[PurposeVersion] = Future.successful(
      PurposeVersion(
        id = UUID.randomUUID(),
        state = PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def waitForApprovalPurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      stateChangeDetails: StateChangeDetails
    )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = Future.successful(
      PurposeVersion(
        id = UUID.randomUUID(),
        state = PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def archivePurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(
      implicit contexts: Seq[(String, String)]
    ): Future[PurposeVersion] = Future.successful(
      PurposeVersion(
        id = UUID.randomUUID(),
        state = PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def updateDraftPurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      updateContent: DraftPurposeVersionUpdateContent
    )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = Future.successful(
      PurposeVersion(
        id = UUID.randomUUID(),
        state = PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def updateWaitingForApprovalPurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      updateContent: WaitingForApprovalPurposeVersionUpdateContent
    )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = Future.successful(
      PurposeVersion(
        id = UUID.randomUUID(),
        state = PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def deletePurpose(purposeId: UUID)(implicit contexts: Seq[(String, String)]): Future[Unit] =
      Future.successful(())

    override def deletePurposeVersion(purposeId: UUID, versionId: UUID)(implicit
      contexts: Seq[(String, String)]
    ): Future[Unit] = Future.successful(())
  }

  class FakeAuthorizationManagementService extends AuthorizationManagementService {
    override def updateStateOnClients(purposeId: UUID, versionId: UUID, state: ClientComponentState)(implicit
      contexts: Seq[(String, String)]
    ): Future[Unit] =
      Future.successful(())

    override def getClients(purposeId: Option[UUID])(implicit contexts: Seq[(String, String)]): Future[Seq[Client]] =
      Future.successful(Seq.empty)

    override def removePurposeFromClient(purposeId: UUID, clientId: UUID)(implicit
      contexts: Seq[(String, String)]
    ): Future[Unit] = Future.successful(())
  }

  class FakeAgreementManagementService extends AgreementManagementService {
    override def getAgreements(eServiceId: UUID, consumerId: UUID, states: Seq[AgreementState])(implicit
      contexts: Seq[(String, String)]
    ): Future[Seq[Agreement]] = Future.successful(Seq.empty)
  }

  class FakeCatalogManagementService extends CatalogManagementService {

    override def getEServiceById(eServiceId: UUID)(implicit contexts: Seq[(String, String)]): Future[EService] =
      Future.successful(
        EService(
          id = UUID.randomUUID(),
          producerId = UUID.randomUUID(),
          name = "fake",
          description = "fake",
          technology = EServiceTechnology.REST,
          attributes = Attributes(Seq.empty, Seq.empty, Seq.empty),
          descriptors = Seq.empty
        )
      )

  }

  class FakeTenantManagementService extends TenantManagementService {
    override def getTenant(tenantId: UUID)(implicit contexts: Seq[(String, String)]): Future[Tenant] =
      Future.successful(
        Tenant(
          id = UUID.randomUUID(),
          kind = TenantKind.PA.some,
          selfcareId = UUID.randomUUID().toString.some,
          externalId = ExternalId("Foo", "Bar"),
          features = Nil,
          attributes = Nil,
          createdAt = OffsetDateTimeSupplier.get(),
          updatedAt = None,
          mails = Nil,
          name = "test_name"
        )
      )
  }
}
