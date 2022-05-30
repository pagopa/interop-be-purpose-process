package it.pagopa.interop.purposeprocess.service.impl

import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.commons.utils.AkkaUtils.getUidFuture
import it.pagopa.interop.purposeprocess.service.{
  PartyManagementApi,
  PartyManagementApiKeyValue,
  PartyManagementInvoker,
  PartyManagementService
}
import it.pagopa.interop.selfcare.partymanagement.client.invoker.ApiRequest
import it.pagopa.interop.selfcare.partymanagement.client.model.{Institution, RelationshipState, Relationships}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class PartyManagementServiceImpl(invoker: PartyManagementInvoker, api: PartyManagementApi)(implicit
  partyManagementApiKeyValue: PartyManagementApiKeyValue
) extends PartyManagementService {

  implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def getInstitutionById(
    institutionId: UUID
  )(implicit contexts: Seq[(String, String)], ec: ExecutionContext): Future[Institution] =
    for {
      uid <- getUidFuture(contexts)
      request: ApiRequest[Institution] = api.getInstitutionById(institutionId)(uid)
      result <- invoker.invoke(request, s"Retrieving Institution $institutionId")
    } yield result

  override def getActiveRelationships(from: UUID, to: UUID)(implicit
    contexts: Seq[(String, String)],
    ec: ExecutionContext
  ): Future[Relationships] = for {
    uid <- getUidFuture(contexts)
    request = api.getRelationships(
      Some(from),
      Some(to),
      roles = Seq.empty,
      states = Seq(RelationshipState.ACTIVE),
      products = Seq.empty, // TODO Should be fixed to interop product
      productRoles = Seq.empty
    )(uid)
    result <- invoker.invoke(request, s"Retrieving active Relationships from $from to $to")
  } yield result
}
