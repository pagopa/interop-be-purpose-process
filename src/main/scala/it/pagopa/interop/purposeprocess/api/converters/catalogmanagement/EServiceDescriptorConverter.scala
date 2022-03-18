package it.pagopa.interop.purposeprocess.api.converters.catalogmanagement

import it.pagopa.interop.catalogmanagement.client.model.{EServiceDescriptor => DependencyEServiceDescriptor}
import it.pagopa.interop.purposeprocess.model.EServiceDescriptor

object EServiceDescriptorConverter {
  def dependencyToApi(descriptor: DependencyEServiceDescriptor): EServiceDescriptor =
    EServiceDescriptor(
      id = descriptor.id,
      version = descriptor.version,
      dailyCalls = descriptor.dailyCallsPerConsumer,
      state = EServiceDescriptorStateConverter.dependencyToApi(descriptor.state)
    )
}
