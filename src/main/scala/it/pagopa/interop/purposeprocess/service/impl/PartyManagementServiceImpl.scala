package it.pagopa.interop.purposeprocess.service.impl

import com.typesafe.scalalogging.{Logger, LoggerTakingImplicit}
import it.pagopa.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.interop.commons.utils.withUid
import it.pagopa.interop.commons.utils.TypeConversions._
import it.pagopa.interop.purposeprocess.service.{
  PartyManagementApi,
  PartyManagementApiKeyValue,
  PartyManagementInvoker,
  PartyManagementService
}
import it.pagopa.interop.selfcare.partymanagement.client.model.{Institution, RelationshipState, Relationships}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

final case class PartyManagementServiceImpl(invoker: PartyManagementInvoker, api: PartyManagementApi)(implicit
  partyManagementApiKeyValue: PartyManagementApiKeyValue
) extends PartyManagementService {

  implicit val logger: LoggerTakingImplicit[ContextFieldsToLog] =
    Logger.takingImplicit[ContextFieldsToLog](this.getClass)

  override def getInstitutionById(
    selfcareId: String
  )(implicit contexts: Seq[(String, String)], ec: ExecutionContext): Future[Institution] = withUid { uid =>
    selfcareId.toFutureUUID.flatMap { selfcareUUID =>
      val request = api.getInstitutionById(selfcareUUID)(uid)
      invoker.invoke(request, s"Retrieving Institution $selfcareUUID")
    }
  }

  override def getActiveRelationships(from: UUID, to: String)(implicit
    contexts: Seq[(String, String)],
    ec: ExecutionContext
  ): Future[Relationships] = withUid(uid =>
    to.toFutureUUID.flatMap { toUUID =>
      val request = api.getRelationships(
        Some(from),
        Some(toUUID),
        roles = Seq.empty,
        states = Seq(RelationshipState.ACTIVE),
        products = Seq.empty, // TODO Should be fixed to interop product
        productRoles = Seq.empty
      )(uid)
      invoker.invoke(request, s"Retrieving active Relationships from $from to $toUUID")
    }
  )
}
