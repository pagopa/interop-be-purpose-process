package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  Purpose,
  PurposeSeed,
  PurposeVersionState,
  Purposes,
  StateChangeDetails
}

import java.util.UUID
import scala.concurrent.Future

trait PurposeManagementService {
  def createPurpose(bearerToken: String)(seed: PurposeSeed): Future[Purpose]
  def getPurpose(bearerToken: String)(id: UUID): Future[Purpose]
  def getPurposes(
    bearerToken: String
  )(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState]): Future[Purposes]

  def suspendPurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[Unit]

  def waitForApprovalPurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[Unit]

  def archivePurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[Unit]
}
