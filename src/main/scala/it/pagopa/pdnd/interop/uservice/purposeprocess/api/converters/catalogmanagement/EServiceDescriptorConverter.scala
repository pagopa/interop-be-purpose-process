package it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.catalogmanagement

import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.{
  EServiceDescriptor => DependencyEServiceDescriptor
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.EServiceDescriptor

object EServiceDescriptorConverter {
  def dependencyToApi(descriptor: DependencyEServiceDescriptor): EServiceDescriptor =
    EServiceDescriptor(
      id = descriptor.id,
      version = descriptor.version,
      dailyCalls = descriptor.dailyCallsMaxNumber,
      state = EServiceDescriptorStateConverter.dependencyToApi(descriptor.state)
    )
}
