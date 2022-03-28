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
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class PurposeManagementServiceImpl(invoker: PurposeManagementInvoker, api: PurposeManagementApi)(implicit
  ec: ExecutionContext
) extends PurposeManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def createPurpose(contexts: Seq[(String, String)])(seed: PurposeSeed): Future[Purpose] = {

    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.createPurpose(xCorrelationId = correlationId, seed, xForwardedFor = ip)(BearerToken(bearerToken))
      result <- invoker.invoke(
        request,
        s"Creating purpose for EService ${seed.eserviceId} and Consumer ${seed.consumerId}"
      )
    } yield result

  }

  override def createPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, seed: PurposeVersionSeed): Future[PurposeVersion] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.createPurposeVersion(xCorrelationId = correlationId, purposeId, seed, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Creating purpose version for Purpose $purposeId")
    } yield result
  }

  override def updatePurpose(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, purposeUpdateContent: PurposeUpdateContent): Future[Purpose] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.updatePurpose(xCorrelationId = correlationId, purposeId, purposeUpdateContent, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Updating Purpose $purposeId")
    } yield result
  }

  override def getPurpose(contexts: Seq[(String, String)])(id: UUID): Future[Purpose] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.getPurpose(xCorrelationId = correlationId, id, xForwardedFor = ip)(BearerToken(bearerToken))
      result <- invoker.invoke(request, s"Retrieving purpose $id")
    } yield result

  }

  override def getPurposes(
    contexts: Seq[(String, String)]
  )(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState]): Future[Purposes] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.getPurposes(xCorrelationId = correlationId, xForwardedFor = ip, eserviceId, consumerId, states)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(
        request,
        s"Retrieving purposes for EService $eserviceId, Consumer $consumerId and States $states"
      )
    } yield result
  }

  override def activatePurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, payload: ActivatePurposeVersionPayload): Future[PurposeVersion] = {
    for {
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

  }

  override def suspendPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion] = {
    for {
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
  }

  override def waitForApprovalPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion] = {
    for {
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
  }

  override def archivePurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion] = {
    for {
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
  }

  override def updateDraftPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, updateContent: DraftPurposeVersionUpdateContent): Future[PurposeVersion] = {
    for {
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
  }

  override def updateWaitingForApprovalPurposeVersion(contexts: Seq[(String, String)])(
    purposeId: UUID,
    versionId: UUID,
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  ): Future[PurposeVersion] = {
    for {
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
  }

  override def deletePurpose(contexts: Seq[(String, String)])(purposeId: UUID): Future[Unit] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.deletePurpose(xCorrelationId = correlationId, purposeId, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Deleting purpose $purposeId")
    } yield result
  }

  override def deletePurposeVersion(contexts: Seq[(String, String)])(purposeId: UUID, versionId: UUID): Future[Unit] = {
    for {
      (bearerToken, correlationId, ip) <- extractHeaders(contexts).toFuture
      request = api.deletePurposeVersion(xCorrelationId = correlationId, purposeId, versionId, xForwardedFor = ip)(
        BearerToken(bearerToken)
      )
      result <- invoker.invoke(request, s"Deleting purpose version $purposeId/$versionId")
    } yield result
  }
}
