package it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl

import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  PurposeManagementApi,
  PurposeManagementInvoker,
  PurposeManagementService
}
import org.slf4j.{Logger, LoggerFactory}

final case class PurposeManagementServiceImpl(invoker: PurposeManagementInvoker, api: PurposeManagementApi)
    extends PurposeManagementService {

  implicit val logger: Logger = LoggerFactory.getLogger(this.getClass)

}
