package it.pagopa.interop.purposeprocess.api.converters.catalogmanagement

import it.pagopa.interop.catalogmanagement.client.model.{EServiceDescriptorState => DependencyEServiceDescriptorState}
import it.pagopa.interop.purposeprocess.model.EServiceDescriptorState

object EServiceDescriptorStateConverter {
  def dependencyToApi(state: DependencyEServiceDescriptorState): EServiceDescriptorState =
    state match {
      case DependencyEServiceDescriptorState.DRAFT      => EServiceDescriptorState.DRAFT
      case DependencyEServiceDescriptorState.PUBLISHED  => EServiceDescriptorState.PUBLISHED
      case DependencyEServiceDescriptorState.DEPRECATED => EServiceDescriptorState.DEPRECATED
      case DependencyEServiceDescriptorState.SUSPENDED  => EServiceDescriptorState.SUSPENDED
      case DependencyEServiceDescriptorState.ARCHIVED   => EServiceDescriptorState.ARCHIVED
    }
}
