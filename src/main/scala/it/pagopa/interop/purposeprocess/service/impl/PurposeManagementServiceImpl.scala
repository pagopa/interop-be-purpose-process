package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.commons.utils.extractHeaders
import it.pagopa.interop.commons.utils.TypeConversions.EitherOps
import it.pagopa.interop.purposemanagement.client.invoker.BearerToken
import it.pagopa.interop.purposemanagement.client.model._
import it.pagopa.interop.purposeprocess.service.{
  PurposeManagementApi,
  PurposeManagementInvoker,
  PurposeManagementService
}
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class PurposeManagementServiceImpl(invoker: PurposeManagementInvoker, api: PurposeManagementApi)(implicit
  ec: ExecutionContext
) extends PurposeManagementService {

  implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def createPurpose(seed: PurposeSeed)(implicit contexts: Seq[(String, String)]): Future[Purpose] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.createPurpose(xCorrelationId = correlationId, seed, xForwardedFor = ip)(BearerToken(bearerToken))
    result <- invoker.invoke(
      request,
      s"Creating purpose for EService ${seed.eserviceId} and Consumer ${seed.consumerId}"
    )
  } yield result

  override def createPurposeVersion(purposeId: UUID, seed: PurposeVersionSeed)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.createPurposeVersion(xCorrelationId = correlationId, purposeId, seed, xForwardedFor = ip)(
      BearerToken(bearerToken)
    )
    result <- invoker.invoke(request, s"Creating purpose version for Purpose $purposeId")
  } yield result

  override def updatePurpose(purposeId: UUID, purposeUpdateContent: PurposeUpdateContent)(implicit
    contexts: Seq[(String, String)]
  ): Future[Purpose] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.updatePurpose(xCorrelationId = correlationId, purposeId, purposeUpdateContent, xForwardedFor = ip)(
      BearerToken(bearerToken)
    )
    result <- invoker.invoke(request, s"Updating Purpose $purposeId")
  } yield result

  override def getPurpose(id: UUID)(implicit contexts: Seq[(String, String)]): Future[Purpose] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.getPurpose(xCorrelationId = correlationId, id, xForwardedFor = ip)(BearerToken(bearerToken))
    result <- invoker.invoke(request, s"Retrieving purpose $id")
  } yield result

  override def getPurposes(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState])(
    implicit contexts: Seq[(String, String)]
  ): Future[Purposes] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.getPurposes(xCorrelationId = correlationId, xForwardedFor = ip, eserviceId, consumerId, states)(
      BearerToken(bearerToken)
    )
    result <- invoker.invoke(
      request,
      s"Retrieving purposes for EService $eserviceId, Consumer $consumerId and States $states"
    )
  } yield result

  override def activatePurposeVersion(purposeId: UUID, versionId: UUID, payload: ActivatePurposeVersionPayload)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.activatePurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      payload,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    result <- invoker.invoke(
      request,
      s"Activating Version $versionId of Purpose $purposeId by ${payload.stateChangeDetails.changedBy}"
    )
  } yield result

  override def suspendPurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.suspendPurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      stateChangeDetails,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    result <- invoker.invoke(
      request,
      s"Suspending Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}"
    )
  } yield result

  override def waitForApprovalPurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(
    implicit contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.waitForApprovalPurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      stateChangeDetails,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    result <- invoker.invoke(
      request,
      s"Waiting for Approval for Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}"
    )
  } yield result

  override def archivePurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.archivePurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      stateChangeDetails,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    result <- invoker.invoke(
      request,
      s"Archiving Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}"
    )
  } yield result

  override def updateDraftPurposeVersion(
    purposeId: UUID,
    versionId: UUID,
    updateContent: DraftPurposeVersionUpdateContent
  )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.updateDraftPurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      updateContent,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    result <- invoker.invoke(request, s"Updating draft version $versionId of Purpose $purposeId with $updateContent")
  } yield result

  override def updateWaitingForApprovalPurposeVersion(
    purposeId: UUID,
    versionId: UUID,
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.updateWaitingForApprovalPurposeVersion(
      xCorrelationId = correlationId,
      purposeId,
      versionId,
      updateContent,
      xForwardedFor = ip
    )(BearerToken(bearerToken))
    result <- invoker.invoke(
      request,
      s"Updating waiting for approval version $versionId of Purpose $purposeId with $updateContent"
    )
  } yield result

  override def deletePurpose(purposeId: UUID)(implicit contexts: Seq[(String, String)]): Future[Unit] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.deletePurpose(xCorrelationId = correlationId, purposeId, xForwardedFor = ip)(BearerToken(bearerToken))
    result <- invoker.invoke(request, s"Deleting purpose $purposeId")
  } yield result

  override def deletePurposeVersion(purposeId: UUID, versionId: UUID)(implicit
    contexts: Seq[(String, String)]
  ): Future[Unit] = for {
    (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
    request = api.deletePurposeVersion(xCorrelationId = correlationId, purposeId, versionId, xForwardedFor = ip)(
      BearerToken(bearerToken)
    )
    result <- invoker.invoke(request, s"Deleting purpose version $purposeId/$versionId")
  } yield result

}
