package it.pagopa.interop.purposeprocess.util

import cats.syntax.all._
import it.pagopa.interop.authorizationmanagement.client.{model => AuthManagement}
import it.pagopa.interop.purposemanagement.client.{model => Management}
import it.pagopa.interop.purposeprocess.service._
import it.pagopa.interop.purposeprocess.common.readmodel.PaginatedResult

import java.time.OffsetDateTime
import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}
import it.pagopa.interop.purposemanagement.model.purpose.{PersistentPurpose, PersistentPurposeVersionState}
import it.pagopa.interop.agreementmanagement.model.agreement.{PersistentAgreementState, PersistentAgreement}
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.catalogmanagement.model.{CatalogAttributes, CatalogItem, Rest, Deliver}
import it.pagopa.interop.tenantmanagement.model.tenant.{PersistentTenant, PersistentExternalId, PersistentTenantKind}
import it.pagopa.interop.authorizationmanagement.model.client.PersistentClient

/**
 * Holds fake implementation of dependencies for tests not requiring neither mocks or stubs
 */
object FakeDependencies {

  class FakePurposeManagementService extends PurposeManagementService {
    override def createPurpose(
      seed: Management.PurposeSeed
    )(implicit contexts: Seq[(String, String)]): Future[Management.Purpose] = Future.successful(
      Management.Purpose(
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
        updatedAt = None,
        isFreeOfCharge = false
      )
    )

    override def createPurposeVersion(purposeId: UUID, seed: Management.PurposeVersionSeed)(implicit
      contexts: Seq[(String, String)]
    ): Future[Management.PurposeVersion] = Future.successful(
      Management.PurposeVersion(
        id = UUID.randomUUID(),
        state = Management.PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def listPurposes(
      requesterId: UUID,
      title: Option[String],
      eServicesIds: List[String],
      consumersIds: List[String],
      producersIds: List[String],
      states: List[PersistentPurposeVersionState],
      excludeDraft: Boolean,
      offset: Int,
      limit: Int,
      exactMatchOnTitle: Boolean = false
    )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[PaginatedResult[PersistentPurpose]] =
      Future.successful(PaginatedResult(Seq.empty, 0))
    override def updatePurpose(purposeId: UUID, purposeUpdateContent: Management.PurposeUpdateContent)(implicit
      contexts: Seq[(String, String)]
    ): Future[Management.Purpose] = Future.successful(
      Management.Purpose(
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
        updatedAt = None,
        isFreeOfCharge = false
      )
    )

    override def getPurposeById(
      purposeId: UUID
    )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[PersistentPurpose] =
      Future.successful(
        PersistentPurpose(
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
          updatedAt = None,
          isFreeOfCharge = false,
          freeOfChargeReason = None
        )
      )

    override def getPurposes(
      eserviceId: Option[UUID],
      consumerId: Option[UUID],
      states: Seq[PersistentPurposeVersionState]
    )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentPurpose]] =
      Future.successful(Seq.empty)

    override def activatePurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      payload: Management.ActivatePurposeVersionPayload
    )(implicit contexts: Seq[(String, String)]): Future[Management.PurposeVersion] = Future.successful(
      Management.PurposeVersion(
        id = UUID.randomUUID(),
        state = Management.PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def suspendPurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      stateChangeDetails: Management.StateChangeDetails
    )(implicit contexts: Seq[(String, String)]): Future[Management.PurposeVersion] = Future.successful(
      Management.PurposeVersion(
        id = UUID.randomUUID(),
        state = Management.PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def waitForApprovalPurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      stateChangeDetails: Management.StateChangeDetails
    )(implicit contexts: Seq[(String, String)]): Future[Management.PurposeVersion] = Future.successful(
      Management.PurposeVersion(
        id = UUID.randomUUID(),
        state = Management.PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def archivePurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      stateChangeDetails: Management.StateChangeDetails
    )(implicit contexts: Seq[(String, String)]): Future[Management.PurposeVersion] = Future.successful(
      Management.PurposeVersion(
        id = UUID.randomUUID(),
        state = Management.PurposeVersionState.DRAFT,
        createdAt = OffsetDateTime.now(),
        dailyCalls = 1
      )
    )

    override def updateWaitingForApprovalPurposeVersion(
      purposeId: UUID,
      versionId: UUID,
      updateContent: Management.WaitingForApprovalPurposeVersionUpdateContent
    )(implicit contexts: Seq[(String, String)]): Future[Management.PurposeVersion] = Future.successful(
      Management.PurposeVersion(
        id = UUID.randomUUID(),
        state = Management.PurposeVersionState.DRAFT,
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
    override def updateStateOnClients(purposeId: UUID, versionId: UUID, state: AuthManagement.ClientComponentState)(
      implicit contexts: Seq[(String, String)]
    ): Future[Unit] =
      Future.successful(())

    override def getClients(
      purposeId: UUID
    )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentClient]] =
      Future.successful(Seq.empty)

    override def removePurposeFromClient(purposeId: UUID, clientId: UUID)(implicit
      contexts: Seq[(String, String)]
    ): Future[Unit] = Future.successful(())
  }

  class FakeAgreementManagementService extends AgreementManagementService {
    override def getAgreements(eServiceId: UUID, consumerId: UUID, states: Seq[PersistentAgreementState])(implicit
      ec: ExecutionContext,
      readModel: ReadModelService
    ): Future[Seq[PersistentAgreement]] = Future.successful(Seq.empty)
  }

  class FakeCatalogManagementService extends CatalogManagementService {

    override def getEServiceById(
      eServiceId: UUID
    )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[CatalogItem] =
      Future.successful(
        CatalogItem(
          id = UUID.randomUUID(),
          producerId = UUID.randomUUID(),
          name = "fake",
          description = "fake",
          technology = Rest,
          attributes = CatalogAttributes.empty.some,
          descriptors = Seq.empty,
          createdAt = OffsetDateTime.now(),
          riskAnalysis = Seq.empty,
          mode = Deliver
        )
      )

  }

  class FakeTenantManagementService extends TenantManagementService {

    override def getTenantById(
      tenantId: UUID
    )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[PersistentTenant] =
      Future.successful(
        PersistentTenant(
          id = UUID.randomUUID(),
          kind = PersistentTenantKind.PA.some,
          selfcareId = UUID.randomUUID().toString.some,
          externalId = PersistentExternalId("Foo", "Bar"),
          features = Nil,
          attributes = Nil,
          createdAt = OffsetDateTime.now(),
          updatedAt = None,
          mails = Nil,
          name = "test_name"
        )
      )
  }
}
