package it.pagopa.interop.purposeprocess.service

import it.pagopa.interop.attributeregistrymanagement.client.model.Attribute

import java.util.UUID
import scala.concurrent.Future

trait AttributeRegistryManagementService {
  def getAttributeById(id: UUID)(implicit contexts: Seq[(String, String)]): Future[Attribute]
}
