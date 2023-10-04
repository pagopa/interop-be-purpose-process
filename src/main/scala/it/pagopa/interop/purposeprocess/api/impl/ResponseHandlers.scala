package it.pagopa.interop.purposeprocess.api.impl

import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.LoggerTakingImplicit
import it.pagopa.interop.commons.logging.ContextFieldsToLog
import it.pagopa.interop.commons.utils.errors.{AkkaResponses, ServiceCode}
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors._

import scala.util.{Failure, Success, Try}

object ResponseHandlers extends AkkaResponses {

  implicit val serviceCode: ServiceCode = ServiceCode("012")

  def retrieveLatestRiskAnalysisConfigurationResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)  => success(s)
      case Failure(ex) => internalServerError(ex, logMessage)
    }

  def retrieveRiskAnalysisConfigurationByVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                     => success(s)
      case Failure(ex: RiskAnalysisConfigVersionNotFound) => notFound(ex, logMessage)
      case Failure(ex)                                    => internalServerError(ex, logMessage)
    }

  def getRiskAnalysisDocumentResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                  => success(s)
      case Failure(ex: OrganizationNotAllowed)         => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)                => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionNotFound)         => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionDocumentNotFound) => notFound(ex, logMessage)
      case Failure(ex)                                 => internalServerError(ex, logMessage)
    }

  def createPurposeResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                  => success(s)
      case Failure(ex: DuplicatedPurposeName)          => conflict(ex, logMessage)
      case Failure(ex: RiskAnalysisValidationFailed)   => badRequest(ex, logMessage)
      case Failure(ex: AgreementNotFound)              => badRequest(ex, logMessage)
      case Failure(ex: MissingFreeOfChargeReason.type) => badRequest(ex, logMessage)
      case Failure(ex: OrganizationIsNotTheConsumer)   => forbidden(ex, logMessage)
      case Failure(ex)                                 => internalServerError(ex, logMessage)
    }

  def createPurposeFromEServiceResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                  => success(s)
      case Failure(ex: EServiceNotFound)               => badRequest(ex, logMessage)
      case Failure(ex: EServiceNotInReceiveMode)       => badRequest(ex, logMessage)
      case Failure(ex: RiskAnalysisNotFound)           => badRequest(ex, logMessage)
      case Failure(ex: AgreementNotFound)              => badRequest(ex, logMessage)
      case Failure(ex: MissingFreeOfChargeReason.type) => badRequest(ex, logMessage)
      case Failure(ex: OrganizationIsNotTheConsumer)   => forbidden(ex, logMessage)
      case Failure(ex: DuplicatedPurposeName)          => conflict(ex, logMessage)
      case Failure(ex)                                 => internalServerError(ex, logMessage)
    }

  def createPurposeVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                => success(s)
      case Failure(ex: OrganizationIsNotTheConsumer) => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)              => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionConflict)       => conflict(ex, logMessage)
      case Failure(ex)                               => internalServerError(ex, logMessage)
    }

  def updatePurposeResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                  => success(s)
      case Failure(ex: RiskAnalysisValidationFailed)   => badRequest(ex, logMessage)
      case Failure(ex: MissingFreeOfChargeReason.type) => badRequest(ex, logMessage)
      case Failure(ex: EServiceNotFound)               => badRequest(ex, logMessage)
      case Failure(ex: EServiceNotInDeliverMode)       => badRequest(ex, logMessage)
      case Failure(ex: TenantNotFound)                 => badRequest(ex, logMessage)
      case Failure(ex: AgreementNotFound)              => badRequest(ex, logMessage)
      case Failure(ex: OrganizationIsNotTheConsumer)   => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotInDraftState)         => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)                => notFound(ex, logMessage)

      case Failure(ex) => internalServerError(ex, logMessage)
    }

  def updateReversePurposeResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                  => success(s)
      case Failure(ex: RiskAnalysisValidationFailed)   => badRequest(ex, logMessage)
      case Failure(ex: MissingFreeOfChargeReason.type) => badRequest(ex, logMessage)
      case Failure(ex: EServiceNotFound)               => badRequest(ex, logMessage)
      case Failure(ex: EServiceNotInReceiveMode)       => badRequest(ex, logMessage)
      case Failure(ex: TenantNotFound)                 => badRequest(ex, logMessage)
      case Failure(ex: AgreementNotFound)              => badRequest(ex, logMessage)
      case Failure(ex: OrganizationIsNotTheConsumer)   => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotInDraftState)         => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)                => notFound(ex, logMessage)

      case Failure(ex) => internalServerError(ex, logMessage)
    }

  def getPurposeResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                   => success(s)
      case Failure(ex: PurposeNotFound) => notFound(ex, logMessage)
      case Failure(ex)                  => internalServerError(ex, logMessage)
    }

  def getPurposesResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)  => success(s)
      case Failure(ex) => internalServerError(ex, logMessage)
    }

  def deletePurposeResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                => success(s)
      case Failure(ex: OrganizationIsNotTheConsumer) => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)              => notFound(ex, logMessage)
      case Failure(ex: PurposeCannotBeDeleted)       => conflict(ex, logMessage)
      case Failure(ex)                               => internalServerError(ex, logMessage)
    }

  def clonePurposeResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                => success(s)
      case Failure(ex: RiskAnalysisValidationFailed) => badRequest(ex, logMessage)
      case Failure(ex: PurposeNotFound)              => notFound(ex, logMessage)
      case Failure(ex: PurposeCannotBeCloned)        => conflict(ex, logMessage)
      case Failure(ex)                               => internalServerError(ex, logMessage)
    }

  def deletePurposeVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                 => success(s)
      case Failure(ex: OrganizationIsNotTheConsumer)  => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)               => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionNotFound)        => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionCannotBeDeleted) => conflict(ex, logMessage)
      case Failure(ex)                                => internalServerError(ex, logMessage)
    }

  def activatePurposeVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                => success(s)
      case Failure(ex: MissingRiskAnalysis)          => badRequest(ex, logMessage)
      case Failure(ex: AgreementNotFound)            => badRequest(ex, logMessage)
      case Failure(ex: RiskAnalysisValidationFailed) => badRequest(ex, logMessage)
      case Failure(ex: OrganizationIsNotTheConsumer) => forbidden(ex, logMessage)
      case Failure(ex: OrganizationIsNotTheProducer) => forbidden(ex, logMessage)
      case Failure(ex: OrganizationNotAllowed)       => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)              => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionNotFound)       => notFound(ex, logMessage)
      case Failure(ex)                               => internalServerError(ex, logMessage)
    }

  def suspendPurposeVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                          => success(s)
      case Failure(ex: OrganizationNotAllowed) => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)        => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionNotFound) => notFound(ex, logMessage)
      case Failure(ex)                         => internalServerError(ex, logMessage)
    }

  def archivePurposeVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                => success(s)
      case Failure(ex: OrganizationIsNotTheConsumer) => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)              => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionNotFound)       => notFound(ex, logMessage)
      case Failure(ex)                               => internalServerError(ex, logMessage)
    }

  def updateDraftPurposeVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                 => success(s)
      case Failure(ex: OrganizationIsNotTheConsumer)  => forbidden(ex, logMessage)
      case Failure(ex: PurposeVersionNotInDraftState) => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)               => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionNotFound)        => notFound(ex, logMessage)
      case Failure(ex)                                => internalServerError(ex, logMessage)
    }

  def updateWaitingForApprovalPurposeVersionResponse[T](logMessage: String)(
    success: T => Route
  )(result: Try[T])(implicit contexts: Seq[(String, String)], logger: LoggerTakingImplicit[ContextFieldsToLog]): Route =
    result match {
      case Success(s)                                => success(s)
      case Failure(ex: OrganizationIsNotTheProducer) => forbidden(ex, logMessage)
      case Failure(ex: PurposeNotFound)              => notFound(ex, logMessage)
      case Failure(ex: PurposeVersionNotFound)       => notFound(ex, logMessage)
      case Failure(ex)                               => internalServerError(ex, logMessage)
    }
}
