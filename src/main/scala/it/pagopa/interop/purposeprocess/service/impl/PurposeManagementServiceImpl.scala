package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.commons.utils.withHeaders
import it.pagopa.interop.purposemanagement.client.invoker.{ApiError, BearerToken}
import it.pagopa.interop.purposemanagement.client.model._
import it.pagopa.interop.purposeprocess.service.{
  PurposeManagementApi,
  PurposeManagementInvoker,
  PurposeManagementService
}
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.{
  PurposeNotFound,
  PurposeVersionConflict,
  PurposeVersionNotFound
}
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.purposeprocess.common.readmodel.{ReadModelPurposeQueries, PaginatedResult}
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.purposemanagement.model.purpose.PersistentPurpose
import it.pagopa.interop.purposemanagement.model.purpose.PersistentPurposeVersionState

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class PurposeManagementServiceImpl(invoker: PurposeManagementInvoker, api: PurposeManagementApi)(implicit
  ec: ExecutionContext
) extends PurposeManagementService {

  implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def createPurpose(seed: PurposeSeed)(implicit contexts: Seq[(String, String)]): Future[Purpose] =
    withHeaders { (bearerToken, correlationId, ip) =>
      val request =
        api.createPurpose(xCorrelationId = correlationId, seed, xForwardedFor = ip)(BearerToken(bearerToken))
      invoker.invoke(request, s"Creating purpose for EService ${seed.eserviceId} and Consumer ${seed.consumerId}")
    }

  override def createPurposeVersion(purposeId: UUID, seed: PurposeVersionSeed)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = withHeaders { (bearerToken, correlationId, ip) =>
    val request = api.createPurposeVersion(xCorrelationId = correlationId, purposeId, seed, xForwardedFor = ip)(
      BearerToken(bearerToken)
    )
    invoker
      .invoke(request, s"Creating purpose version for Purpose $purposeId")
      .recoverWith { case err: ApiError[_] if err.code == 409 => Future.failed(PurposeVersionConflict(purposeId)) }
  }

  override def updatePurpose(purposeId: UUID, purposeUpdateContent: PurposeUpdateContent)(implicit
    contexts: Seq[(String, String)]
  ): Future[Purpose] = withHeaders { (bearerToken, correlationId, ip) =>
    val request =
      api.updatePurpose(xCorrelationId = correlationId, purposeId, purposeUpdateContent, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
    invoker.invoke(request, s"Updating Purpose $purposeId")
  }

  override def getPurposeById(
    purposeId: UUID
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[PersistentPurpose] =
    ReadModelPurposeQueries.getPurpose(purposeId).flatMap(_.toFuture(PurposeNotFound(purposeId)))

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
    ReadModelPurposeQueries.listPurposes(
      requesterId,
      title,
      eServicesIds,
      consumersIds,
      producersIds,
      states,
      excludeDraft,
      offset,
      limit,
      exactMatchOnTitle
    )

  override def getPurposes(
    eserviceId: Option[UUID],
    consumerId: Option[UUID],
    states: Seq[PersistentPurposeVersionState]
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentPurpose]] =
    getAllPurposes(eserviceId, consumerId, states)

  private def getPurposes(
    eserviceId: Option[UUID],
    consumerId: Option[UUID],
    states: Seq[PersistentPurposeVersionState],
    offset: Int,
    limit: Int
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentPurpose]] =
    ReadModelPurposeQueries.getPurposes(eserviceId, consumerId, states, offset, limit)

  private def getAllPurposes(
    eserviceId: Option[UUID],
    consumerId: Option[UUID],
    states: Seq[PersistentPurposeVersionState]
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentPurpose]] = {

    def getPurposeFrom(offset: Int): Future[Seq[PersistentPurpose]] =
      getPurposes(eserviceId, consumerId, states, offset = offset, limit = 50)

    def go(start: Int)(as: Seq[PersistentPurpose]): Future[Seq[PersistentPurpose]] =
      getPurposeFrom(start).flatMap(recs =>
        if (recs.size < 50) Future.successful(as ++ recs) else go(start + 50)(as ++ recs)
      )

    go(0)(Nil)
  }

  override def activatePurposeVersion(purposeId: UUID, versionId: UUID, payload: ActivatePurposeVersionPayload)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = withHeaders { (bearerToken, correlationId, ip) =>
    val request =
      api.activatePurposeVersion(xCorrelationId = correlationId, purposeId, versionId, payload, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
    invoker.invoke(
      request,
      s"Activating Version $versionId of Purpose $purposeId by ${payload.stateChangeDetails.changedBy}"
    )
  }

  override def suspendPurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = withHeaders { (bearerToken, correlationId, ip) =>
    val request = api.suspendPurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      stateChangeDetails,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    invoker.invoke(request, s"Suspending Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}")
  }

  override def waitForApprovalPurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(
    implicit contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = withHeaders { (bearerToken, correlationId, ip) =>
    val request = api.waitForApprovalPurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      stateChangeDetails,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    invoker.invoke(
      request,
      s"Waiting for Approval for Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}"
    )
  }

  override def archivePurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = withHeaders { (bearerToken, correlationId, ip) =>
    val request = api.archivePurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      stateChangeDetails,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    invoker.invoke(request, s"Archiving Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}")
  }

  override def updateWaitingForApprovalPurposeVersion(
    purposeId: UUID,
    versionId: UUID,
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = withHeaders {
    (bearerToken, correlationId, ip) =>
      val request = api.updateWaitingForApprovalPurposeVersion(
        xCorrelationId = correlationId,
        purposeId,
        versionId,
        updateContent,
        xForwardedFor = ip
      )(BearerToken(bearerToken))
      invoker.invoke(
        request,
        s"Updating waiting for approval version $versionId of Purpose $purposeId with $updateContent"
      )
  }

  override def deletePurpose(purposeId: UUID)(implicit contexts: Seq[(String, String)]): Future[Unit] = withHeaders {
    (bearerToken, correlationId, ip) =>
      val request =
        api.deletePurpose(xCorrelationId = correlationId, purposeId, xForwardedFor = ip)(BearerToken(bearerToken))
      invoker.invoke(request, s"Deleting purpose $purposeId")
  }

  override def deletePurposeVersion(purposeId: UUID, versionId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Unit] = withHeaders { (bearerToken, correlationId, ip) =>
    val request = api.deletePurposeVersion(xCorrelationId = correlationId, purposeId, versionId, xForwardedFor = ip)(
      BearerToken(bearerToken)
    )
    invoker
      .invoke(request, s"Deleting purpose version $purposeId/$versionId")
      .recoverWith {
        case err: ApiError[_] if err.code == 404 => Future.failed(PurposeVersionNotFound(purposeId, versionId))
      }
  }

}
