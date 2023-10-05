package it.pagopa.interop.purposeprocess.api

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import it.pagopa.interop.commons.utils.SprayCommonFormats.{offsetDateTimeFormat, uuidFormat}
import it.pagopa.interop.purposeprocess.model._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

package object impl extends SprayJsonSupport with DefaultJsonProtocol {

  implicit def clientFormat: RootJsonFormat[Client]                 = jsonFormat2(Client)
  implicit def organizationFormat: RootJsonFormat[Organization]     = jsonFormat2(Organization)
  implicit def descriptorFormat: RootJsonFormat[EServiceDescriptor] = jsonFormat4(EServiceDescriptor)
  implicit def eServiceFormat: RootJsonFormat[EService]             = jsonFormat4(EService)
  implicit def agreementFormat: RootJsonFormat[Agreement]           = jsonFormat2(Agreement)

  implicit def riskAnalysisFormFormat: RootJsonFormat[RiskAnalysisForm]             = jsonFormat3(RiskAnalysisForm)
  implicit def purposeVersionDocumentFormat: RootJsonFormat[PurposeVersionDocument] =
    jsonFormat4(PurposeVersionDocument)
  implicit def purposeFormat: RootJsonFormat[Purpose]                               = jsonFormat14(Purpose)
  implicit def purposeVersionFormat: RootJsonFormat[PurposeVersion]                 = jsonFormat9(PurposeVersion)
  implicit def purposesFormat: RootJsonFormat[Purposes]                             = jsonFormat2(Purposes)
  implicit def purposeSeedFormat: RootJsonFormat[PurposeSeed]                       = jsonFormat8(PurposeSeed)
  implicit def purposeUpdateContentFormat: RootJsonFormat[PurposeUpdateContent]     = jsonFormat6(PurposeUpdateContent)
  implicit def reversePurposeUpdateContentFormat: RootJsonFormat[ReversePurposeUpdateContent] =
    jsonFormat5(ReversePurposeUpdateContent)
  implicit def purposeVersionSeedFormat: RootJsonFormat[PurposeVersionSeed]       = jsonFormat1(PurposeVersionSeed)
  implicit def problemErrorFormat: RootJsonFormat[ProblemError]                   = jsonFormat2(ProblemError)
  implicit def problemFormat: RootJsonFormat[Problem]                             = jsonFormat6(Problem)
  implicit def waitingForApprovalPurposeVersionUpdateFormat
    : RootJsonFormat[WaitingForApprovalPurposeVersionUpdateContent] =
    jsonFormat1(WaitingForApprovalPurposeVersionUpdateContent)
  implicit def dependencyResponseFormat: RootJsonFormat[DependencyResponse]       =
    jsonFormat2(DependencyResponse)
  implicit def localizedTextResponseFormat: RootJsonFormat[LocalizedTextResponse] =
    jsonFormat2(LocalizedTextResponse)
  implicit def labeledValueResponseFormat: RootJsonFormat[LabeledValueResponse]   =
    jsonFormat2(LabeledValueResponse)
  implicit def hideOptionResponseFormat: RootJsonFormat[HideOptionResponse]       =
    jsonFormat2(HideOptionResponse)
  implicit def validationOptionResponseFormat: RootJsonFormat[ValidationOptionResponse]             =
    jsonFormat1(ValidationOptionResponse)
  implicit def formConfigQuestionResponseFormat: RootJsonFormat[FormConfigQuestionResponse]         =
    jsonFormat11(FormConfigQuestionResponse)
  implicit def riskAnalysisFormConfigResponseFormat: RootJsonFormat[RiskAnalysisFormConfigResponse] =
    jsonFormat2(RiskAnalysisFormConfigResponse)
  implicit def eServicePurposeSeedFormat: RootJsonFormat[EServicePurposeSeed] = jsonFormat8(EServicePurposeSeed)

  final val entityMarshallerProblem: ToEntityMarshaller[Problem] = sprayJsonMarshaller[Problem]
}
