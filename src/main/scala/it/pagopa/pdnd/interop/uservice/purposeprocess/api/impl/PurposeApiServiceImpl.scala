package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.Logger
import it.pagopa.pdnd.interop.commons.jwt.service.JWTReader
import it.pagopa.pdnd.interop.commons.logging.{CanLogContextFields, ContextFieldsToLog}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.ProcessApiService
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.PurposeManagementService
import org.slf4j.LoggerFactory

final case class PurposeApiServiceImpl(purposeManagementService: PurposeManagementService, jwtReader: JWTReader)
    extends ProcessApiService {
  val logger = Logger.takingImplicit[ContextFieldsToLog](LoggerFactory.getLogger(this.getClass))

  /** Code: 204, Message: Placeholder
    */
  override def placeholder()(implicit contexts: Seq[(String, String)]): Route = ???
}
