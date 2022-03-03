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
  def createPurpose(bearerToken: String)(seed: PurposeSeed): Future[Purpose]
  def createPurposeVersion(bearerToken: String)(purposeId: UUID, seed: PurposeVersionSeed): Future[PurposeVersion]
  def updatePurpose(bearerToken: String)(purposeId: UUID, purposeUpdateContent: PurposeUpdateContent): Future[Purpose]
  def getPurpose(bearerToken: String)(id: UUID): Future[Purpose]
  def getPurposes(
    bearerToken: String
  )(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState]): Future[Purposes]

  def activatePurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, payload: ActivatePurposeVersionPayload): Future[PurposeVersion]

  def suspendPurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion]

  def waitForApprovalPurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion]

  def archivePurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[PurposeVersion]

  def updateDraftPurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, updateContent: DraftPurposeVersionUpdateContent): Future[PurposeVersion]

  def updateWaitingForApprovalPurposeVersion(bearerToken: String)(
    purposeId: UUID,
    versionId: UUID,
    updateContent: WaitingForApprovalPurposeVersionUpdateContent
  ): Future[PurposeVersion]

  def deletePurpose(bearerToken: String)(purposeId: UUID): Future[Unit]

  def deletePurposeVersion(bearerToken: String)(purposeId: UUID, versionId: UUID): Future[Unit]
}
