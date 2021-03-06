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
  def createPurpose(seed: PurposeSeed)(implicit contexts: Seq[(String, String)]): Future[Purpose]
  def createPurposeVersion(purposeId: UUID, seed: PurposeVersionSeed)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion]
  def updatePurpose(purposeId: UUID, purposeUpdateContent: PurposeUpdateContent)(implicit
    contexts: Seq[(String, String)]
  ): Future[Purpose]
  def getPurpose(id: UUID)(implicit contexts: Seq[(String, String)]): Future[Purpose]
  def getPurposes(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState])(implicit
    contexts: Seq[(String, String)]
  ): Future[Purposes]

  def activatePurposeVersion(purposeId: UUID, versionId: UUID, payload: ActivatePurposeVersionPayload)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion]

  def suspendPurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion]

  def waitForApprovalPurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion]

  def archivePurposeVersion(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails)(implicit
    contexts: Seq[(String, String)]
  ): Future[PurposeVersion]

  def updateDraftPurposeVersion(purposeId: UUID, versionId: UUID, updateContent: DraftPurposeVersionUpdateContent)(
    implicit contexts: Seq[(String, String)]
  ): Future[PurposeVersion]

  def updateWaitingForApprovalPurposeVersion(
    purposeId: UUID,
    versionId: UUID,
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  )(implicit contexts: Seq[(String, String)]): Future[PurposeVersion]

  def deletePurpose(purposeId: UUID)(implicit contexts: Seq[(String, String)]): Future[Unit]

  def deletePurposeVersion(purposeId: UUID, versionId: UUID)(implicit contexts: Seq[(String, String)]): Future[Unit]
}
