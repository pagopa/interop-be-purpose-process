package it.pagopa.pdnd.interop.uservice.purposeprocess.error

import it.pagopa.pdnd.interop.commons.utils.errors.ComponentError

object PurposeProcessErrors {

  final case class ActivatePurposeError(purposeId: String)
      extends ComponentError("0001", s"Error while activating purpose $purposeId")

}
