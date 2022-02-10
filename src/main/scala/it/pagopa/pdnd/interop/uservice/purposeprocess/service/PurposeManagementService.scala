package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  ActivatePurposeVersionPayload,
  Purpose,
  PurposeSeed,
  PurposeVersion,
  PurposeVersionSeed,
  PurposeVersionState,
  Purposes,
  StateChangeDetails
}

import java.util.UUID
import scala.concurrent.Future

trait PurposeManagementService {
  def createPurpose(bearerToken: String)(seed: PurposeSeed): Future[Purpose]
  def createPurposeVersion(bearerToken: String)(purposeId: UUID, seed: PurposeVersionSeed): Future[PurposeVersion]
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
}
