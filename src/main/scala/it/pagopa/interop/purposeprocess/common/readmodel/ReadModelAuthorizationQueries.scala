package it.pagopa.interop.purposeprocess.common.readmodel

import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.authorizationmanagement.model.persistence.JsonFormats._
import it.pagopa.interop.authorizationmanagement.model.client.PersistentClient
import org.mongodb.scala.model.Filters

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

object ReadModelAuthorizationQueries extends ReadModelQuery {
  def getClients(purposeId: UUID, offset: Int, limit: Int)(implicit
    ec: ExecutionContext,
    readModel: ReadModelService
  ): Future[Seq[PersistentClient]] = readModel
    .find[PersistentClient]("clients", Filters.eq("data.purposes.purpose.purposeId", purposeId.toString), offset, limit)
}
