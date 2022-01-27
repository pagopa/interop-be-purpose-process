package it.pagopa.pdnd.interop.uservice.purposeprocess.service

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{Purpose, PurposeSeed}

import scala.concurrent.Future

trait PurposeManagementService {
  def createPurpose(bearerToken: String)(seed: PurposeSeed): Future[Purpose]

}
