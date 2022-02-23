package it.pagopa.interop.purposeprocess.service.impl

import it.pagopa.interop.partymanagement.client.invoker.{ApiRequest, BearerToken}
import it.pagopa.interop.partymanagement.client.model.{Organization, RelationshipState, Relationships}
import it.pagopa.interop.purposeprocess.service.{PartyManagementApi, PartyManagementInvoker, PartyManagementService}
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.Future

final case class PartyManagementServiceImpl(invoker: PartyManagementInvoker, api: PartyManagementApi)
    extends PartyManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def getOrganizationById(bearerToken: String)(organizationId: UUID): Future[Organization] = {
    val request: ApiRequest[Organization] = api.getOrganizationById(organizationId)(BearerToken(bearerToken))
    invoker.invoke(request, s"Retrieving Organization $organizationId")
  }

  override def getActiveRelationships(bearerToken: String)(from: UUID, to: UUID): Future[Relationships] = {
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
