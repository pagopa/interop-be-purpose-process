package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.partymanagement.client.invoker.{ApiRequest, BearerToken}
import it.pagopa.interop.partymanagement.client.model.{Institution, RelationshipState, Relationships}
import it.pagopa.interop.purposeprocess.service.{PartyManagementApi, PartyManagementInvoker, PartyManagementService}
import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}

import java.util.UUID
import scala.concurrent.Future

final case class PartyManagementServiceImpl(invoker: PartyManagementInvoker, api: PartyManagementApi)
    extends PartyManagementService {

  implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def getInstitutionById(
    bearerToken: String
  )(institutionId: UUID)(implicit contexts: Seq[(String, String)]): Future[Institution] = {
    val request: ApiRequest[Institution] = api.getInstitutionById(institutionId)(BearerToken(bearerToken))
    invoker.invoke(request, s"Retrieving Institution $institutionId")
  }

  override def getActiveRelationships(
    bearerToken: String
  )(from: UUID, to: UUID)(implicit contexts: Seq[(String, String)]): Future[Relationships] = {
    val request: ApiRequest[Relationships] = api.getRelationships(
      Some(from),
      Some(to),
      roles = Seq.empty,
      states = Seq(RelationshipState.ACTIVE),
      products = Seq.empty, // TODO Should be fixed to interop product
      productRoles = Seq.empty
    )(BearerToken(bearerToken))
    invoker.invoke(request, s"Retrieving active Relationships from $from to $to")
  }
}
