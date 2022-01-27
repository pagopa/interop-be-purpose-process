package it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.invoker.{ApiRequest, BearerToken}
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  Purpose,
  PurposeSeed,
  PurposeVersionState,
  Purposes,
  StateChangeDetails
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  PurposeManagementApi,
  PurposeManagementInvoker,
  PurposeManagementService
}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.Future

final case class PurposeManagementServiceImpl(invoker: PurposeManagementInvoker, api: PurposeManagementApi)
    extends PurposeManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def createPurpose(bearerToken: String)(seed: PurposeSeed): Future[Purpose] = {
    val request: ApiRequest[Purpose] = api.createPurpose(seed)(BearerToken(bearerToken))
    invoker.invoke(request, s"Creating purpose for EService ${seed.eserviceId} and Consumer ${seed.consumerId}")
  }

  override def getPurpose(bearerToken: String)(id: UUID): Future[Purpose] = {
    val request: ApiRequest[Purpose] = api.getPurpose(id)(BearerToken(bearerToken))
    invoker.invoke(request, s"Retrieving purpose $id")
  }

  override def getPurposes(
    bearerToken: String
  )(eserviceId: Option[UUID], consumerId: Option[UUID], states: Seq[PurposeVersionState]): Future[Purposes] = {
    val request: ApiRequest[Purposes] = api.getPurposes(eserviceId, consumerId, states)(BearerToken(bearerToken))
    invoker.invoke(request, s"Retrieving purposes for EService $eserviceId, Consumer $consumerId and States $states")
  }

  override def suspendPurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[Unit] = {
    val request: ApiRequest[Unit] =
      api.suspendPurposeVersion(purposeId, versionId, stateChangeDetails)(BearerToken(bearerToken))
    invoker.invoke(request, s"Suspending Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}")
  }

  override def waitForApprovalPurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[Unit] = {
    val request: ApiRequest[Unit] =
      api.waitForApprovalPurposeVersion(purposeId, versionId, stateChangeDetails)(BearerToken(bearerToken))
    invoker.invoke(
      request,
      s"Waiting for Approval for Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}"
    )
  }

  override def archivePurposeVersion(
    bearerToken: String
  )(purposeId: UUID, versionId: UUID, stateChangeDetails: StateChangeDetails): Future[Unit] = {
    val request: ApiRequest[Unit] =
      api.archivePurposeVersion(purposeId, versionId, stateChangeDetails)(BearerToken(bearerToken))
    invoker.invoke(request, s"Archiving Version $versionId of Purpose $purposeId by ${stateChangeDetails.changedBy}")
  }
}
