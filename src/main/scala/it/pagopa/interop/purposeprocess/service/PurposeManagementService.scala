package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.purposemanagement.client.model.{
  ActivatePurposeVersionPayload,
  Purpose,
  PurposeSeed,
  PurposeUpdateContent,
  PurposeVersion,
  PurposeVersionSeed,
  PurposeVersionState,
  Purposes,
  StateChangeDetails,
  DraftPurposeVersionUpdateContent,
  WaitingForApprovalPurposeVersionUpdateContent
}

import java.util.UUID
import scala.concurrent.Future

trait PurposeManagementService {
  def createPurpose(contexts: Seq[(String, String)])(seed: PurposeSeed): Future[Purpose]
  def createPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, seed: PurposeVersionSeed): Future[PurposeVersion]
  def updatePurpose(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, purposeUpdateContent: PurposeUpdateContent): Future[Purpose]
  def getPurpose(contexts: Seq[(String, String)])(id: UUID): Future[Purpose]
  def getPurposes(
    contexts: Seq[(String, String)]
  )(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState]): Future[Purposes]

  def activatePurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, payload: ActivatePurposeVersionPayload): Future[PurposeVersion]

  def suspendPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion]

  def waitForApprovalPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion]

  def archivePurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion]

  def updateDraftPurposeVersion(
    contexts: Seq[(String, String)]
  )(purposeId: UUID, versionId: UUID, updateContent: DraftPurposeVersionUpdateContent): Future[PurposeVersion]

  def updateWaitingForApprovalPurposeVersion(contexts: Seq[(String, String)])(
    purposeId: UUID,
    versionId: UUID,
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  ): Future[PurposeVersion]

  def deletePurpose(contexts: Seq[(String, String)])(purposeId: UUID): Future[Unit]

  def deletePurposeVersion(contexts: Seq[(String, String)])(purposeId: UUID, versionId: UUID): Future[Unit]
}
