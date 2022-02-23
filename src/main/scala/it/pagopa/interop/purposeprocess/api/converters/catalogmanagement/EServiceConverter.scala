package it.pagopa.interop.purposeprocess.api.converters.catalogmanagement

import it.pagopa.interop.catalogmanagement.client.model.{EService => DependencyEService}
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.DescriptorNotFound
import it.pagopa.interop.purposeprocess.model.{EService, Organization}

import java.util.UUID

object EServiceConverter {
  def dependencyToApi(
    eService: DependencyEService,
    descriptorId: UUID,
    producer: Organization
  ): Either[Throwable, EService] = {
    for {
      depDescriptor <- eService.descriptors
        .find(_.id == descriptorId)
        .toRight(DescriptorNotFound(eService.id.toString, descriptorId.toString))
      descriptor = EServiceDescriptorConverter.dependencyToApi(depDescriptor)
    } yield EService(id = eService.id, name = eService.name, producer = producer, descriptor = descriptor)
  }
}
