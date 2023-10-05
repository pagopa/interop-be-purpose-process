package it.pagopa.interop.purposeprocess.api

import cats.syntax.all._
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposemanagement.client.{model => Management}
import it.pagopa.interop.purposemanagement.model.{purpose => Persistent}
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind
import it.pagopa.interop.purposeprocess.error.PurposeProcessErrors.RiskAnalysisValidationFailed
import it.pagopa.interop.commons.riskanalysis.model.riskAnalysisTemplate.{
  DataType,
  Dependency,
  FormConfigQuestion,
  FreeInputQuestion,
  FreeText,
  HideOptionConfig,
  LabeledValue,
  LocalizedText,
  Multi,
  MultiQuestion,
  RiskAnalysisFormConfig,
  Single,
  SingleQuestion,
  ValidationOption
}
import it.pagopa.interop.commons.riskanalysis.api.impl.RiskAnalysisValidation
import it.pagopa.interop.commons.riskanalysis.{model => Template}
import java.util.UUID
import it.pagopa.interop.catalogmanagement.model.CatalogRiskAnalysisForm
import it.pagopa.interop.catalogmanagement.model.CatalogRiskAnalysisSingleAnswer
import it.pagopa.interop.catalogmanagement.model.CatalogRiskAnalysisMultiAnswer

object Adapters {

  implicit class PersistentPurposeVersionDocumentWrapper(
    private val document: Persistent.PersistentPurposeVersionDocument
  ) extends AnyVal {
    def toApi: PurposeVersionDocument =
      PurposeVersionDocument(
        id = document.id,
        contentType = document.contentType,
        path = document.path,
        createdAt = document.createdAt
      )

    def toManagement: Management.PurposeVersionDocument =
      Management.PurposeVersionDocument(
        id = document.id,
        contentType = document.contentType,
        path = document.path,
        createdAt = document.createdAt
      )
  }

  implicit class ManagementPurposeVersionDocumentWrapper(private val document: Management.PurposeVersionDocument)
      extends AnyVal {
    def toApi: PurposeVersionDocument =
      PurposeVersionDocument(
        id = document.id,
        contentType = document.contentType,
        path = document.path,
        createdAt = document.createdAt
      )

    def toPersistent: Persistent.PersistentPurposeVersionDocument =
      Persistent.PersistentPurposeVersionDocument(
        id = document.id,
        contentType = document.contentType,
        path = document.path,
        createdAt = document.createdAt
      )
  }

  implicit class TemplateRiskAnalysisFormSeedWrapper(private val riskAnalysis: Template.RiskAnalysisFormSeed)
      extends AnyVal {
    def toManagement(riskAnalysisId: Option[UUID]): Management.RiskAnalysisFormSeed =
      Management.RiskAnalysisFormSeed(
        riskAnalysisId = riskAnalysisId,
        version = riskAnalysis.version,
        singleAnswers = riskAnalysis.singleAnswers.map(_.toManagement),
        multiAnswers = riskAnalysis.multiAnswers.map(_.toManagement)
      )
  }

  implicit class RiskAnalysisSingleAnswerValidatedWrapper(
    private val singleAnswers: Template.RiskAnalysisSingleAnswerValidated
  ) extends AnyVal {
    def toManagement: Management.RiskAnalysisSingleAnswerSeed =
      Management.RiskAnalysisSingleAnswerSeed(key = singleAnswers.key, value = singleAnswers.value)
  }

  implicit class RiskAnalysisMultiAnswerValidatedWrapper(
    private val multiAnswers: Template.RiskAnalysisMultiAnswerValidated
  ) extends AnyVal {
    def toManagement: Management.RiskAnalysisMultiAnswerSeed =
      Management.RiskAnalysisMultiAnswerSeed(key = multiAnswers.key, values = multiAnswers.values)
  }

  implicit class PurposeSeedWrapper(private val seed: PurposeSeed) extends AnyVal {
    def toManagement(
      schemaOnlyValidation: Boolean
    )(kind: PersistentTenantKind): Either[RiskAnalysisValidationFailed, Management.PurposeSeed] =
      for {
        riskAnalysisFormSeed <- seed.riskAnalysisForm
          .traverse(risk =>
            RiskAnalysisValidation
              .validate(risk.toTemplate, schemaOnlyValidation)(kind.toTemplate)
              .leftMap(RiskAnalysisValidationFailed(_))
              .toEither
              .map(_.toManagement(risk.riskAnalysisId))
          )
      } yield Management.PurposeSeed(
        eserviceId = seed.eserviceId,
        consumerId = seed.consumerId,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = riskAnalysisFormSeed,
        isFreeOfCharge = seed.isFreeOfCharge,
        freeOfChargeReason = seed.freeOfChargeReason,
        dailyCalls = seed.dailyCalls
      )
  }

  implicit class PurposeProducerSeedWrapper(private val seed: EServicePurposeSeed) extends AnyVal {
    def toManagement(eserviceId: UUID, riskAnalysisForm: Management.RiskAnalysisForm): Management.PurposeSeed =
      Management.PurposeSeed(
        eserviceId = eserviceId,
        consumerId = seed.consumerId,
        title = seed.title,
        description = seed.description,
        riskAnalysisForm = Some(
          Management.RiskAnalysisFormSeed(
            riskAnalysisId = Some(seed.riskAnalysisId),
            version = riskAnalysisForm.version,
            singleAnswers = riskAnalysisForm.singleAnswers.map(_.toSeed),
            multiAnswers = riskAnalysisForm.multiAnswers.map(_.toSeed)
          )
        ),
        isFreeOfCharge = seed.isFreeOfCharge,
        freeOfChargeReason = seed.freeOfChargeReason,
        dailyCalls = seed.dailyCalls
      )
  }

  implicit class CatalogRiskAnalysisFormWrapper(private val riskAnalysisForm: CatalogRiskAnalysisForm) extends AnyVal {
    def toManagement(riskAnalysisId: UUID): Management.RiskAnalysisForm = Management.RiskAnalysisForm(
      id = riskAnalysisForm.id,
      riskAnalysisId = Some(riskAnalysisId),
      version = riskAnalysisForm.version,
      singleAnswers = riskAnalysisForm.singleAnswers.map(_.toManagement),
      multiAnswers = riskAnalysisForm.multiAnswers.map(_.toManagement)
    )
  }

  implicit class CatalogRiskAnalysisMultiAnswerWrapper(private val multiAnswer: CatalogRiskAnalysisMultiAnswer)
      extends AnyVal {
    def toManagement: Management.RiskAnalysisMultiAnswer =
      Management.RiskAnalysisMultiAnswer(id = multiAnswer.id, key = multiAnswer.key, values = multiAnswer.values)
  }

  implicit class CatalogRiskAnalysisSingleAnswerWrapper(private val singleAnswer: CatalogRiskAnalysisSingleAnswer)
      extends AnyVal {
    def toManagement: Management.RiskAnalysisSingleAnswer =
      Management.RiskAnalysisSingleAnswer(id = singleAnswer.id, key = singleAnswer.key, value = singleAnswer.value)
  }

  implicit class ManagementRiskAnalysisWrapper(private val riskAnalysis: Management.RiskAnalysisForm) extends AnyVal {
    def toApi: RiskAnalysisForm =
      RiskAnalysisForm(
        version = riskAnalysis.version,
        answers = riskAnalysis.singleAnswers.toApi ++ riskAnalysis.multiAnswers.toApi,
        riskAnalysisId = riskAnalysis.riskAnalysisId
      )
  }

  implicit class ManagementSingleAnswerWrapper(private val singleAnswer: Management.RiskAnalysisSingleAnswer)
      extends AnyVal {
    def toSeed: Management.RiskAnalysisSingleAnswerSeed =
      Management.RiskAnalysisSingleAnswerSeed(key = singleAnswer.key, value = singleAnswer.value)
  }

  implicit class ManagementMultinswerWrapper(private val multiAnswer: Management.RiskAnalysisMultiAnswer)
      extends AnyVal {
    def toSeed: Management.RiskAnalysisMultiAnswerSeed =
      Management.RiskAnalysisMultiAnswerSeed(key = multiAnswer.key, values = multiAnswer.values)
  }

  implicit class ManagementSingleAnswersWrapper(private val singleAnswers: Seq[Management.RiskAnalysisSingleAnswer])
      extends AnyVal {
    def toApi: Map[String, Seq[String]] = singleAnswers.map(a => (a.key, a.value.toSeq)).toMap
  }

  implicit class ManagementMultiAnswersWrapper(private val multiAnswers: Seq[Management.RiskAnalysisMultiAnswer])
      extends AnyVal {
    def toApi: Map[String, Seq[String]] = multiAnswers.map(a => (a.key, a.values)).toMap
  }

  implicit class RiskAnalysisFormWrapper(private val riskAnalysis: RiskAnalysisForm) extends AnyVal {
    def toTemplate: Template.RiskAnalysisForm =
      Template.RiskAnalysisForm(version = riskAnalysis.version, answers = riskAnalysis.answers)
  }

  implicit class TenantKindWrapper(private val kind: TenantKind) extends AnyVal {
    def toPersistent: PersistentTenantKind = kind match {
      case TenantKind.PA      => PersistentTenantKind.PA
      case TenantKind.GSP     => PersistentTenantKind.GSP
      case TenantKind.PRIVATE => PersistentTenantKind.PRIVATE
    }
  }

  implicit class PersistentTenantKindrapper(private val kind: PersistentTenantKind) extends AnyVal {
    def toTemplate: Template.RiskAnalysisTenantKind = kind match {
      case PersistentTenantKind.PA      => Template.RiskAnalysisTenantKind.PA
      case PersistentTenantKind.GSP     => Template.RiskAnalysisTenantKind.GSP
      case PersistentTenantKind.PRIVATE => Template.RiskAnalysisTenantKind.PRIVATE
    }
  }

  implicit class PersistentRiskAnalysisFormWrapper(private val riskAnalysis: Persistent.PersistentRiskAnalysisForm)
      extends AnyVal {
    def toApi: RiskAnalysisForm =
      RiskAnalysisForm(
        version = riskAnalysis.version,
        answers = riskAnalysis.singleAnswers.toApi ++ riskAnalysis.multiAnswers.toApi,
        riskAnalysisId = riskAnalysis.riskAnalysisId
      )
  }

  implicit class PersistentRiskAnalysisSingleAnswerWrapper(
    private val singleAnswers: Seq[Persistent.PersistentRiskAnalysisSingleAnswer]
  ) extends AnyVal {
    def toApi: Map[String, Seq[String]] = singleAnswers.map(a => (a.key, a.value.toSeq)).toMap
  }

  implicit class PersistentRiskAnalysisMultiAnswerWrapper(
    private val multiAnswers: Seq[Persistent.PersistentRiskAnalysisMultiAnswer]
  ) extends AnyVal {
    def toApi: Map[String, Seq[String]] = multiAnswers.map(a => (a.key, a.values)).toMap
  }

  implicit class RiskAnalysisFormConfigWrapper(private val riskAnalysisFormConfig: RiskAnalysisFormConfig)
      extends AnyVal {
    def toApi: RiskAnalysisFormConfigResponse =
      RiskAnalysisFormConfigResponse(
        version = riskAnalysisFormConfig.version,
        questions = riskAnalysisFormConfig.questions.map(_.toApi)
      )
  }

  implicit class FormConfigQuestionWrapper(private val question: FormConfigQuestion) extends AnyVal {
    def toApi: FormConfigQuestionResponse = question match {
      case FreeInputQuestion(
            id,
            label,
            infoLabel,
            dataType,
            required,
            dependencies,
            externalType,
            defaultValue,
            hideOption,
            validation
          ) =>
        FormConfigQuestionResponse(
          id = id,
          label = label.toApi,
          infoLabel = infoLabel.map(_.toApi),
          dataType = dataType.toApi,
          required = required,
          dependencies = dependencies.map(_.toApi),
          visualType = externalType,
          defaultValue = defaultValue,
          hideOption = hideOption.map(_.toApi),
          validation = validation.map(_.toApi)
        )
      case SingleQuestion(
            id,
            label,
            infoLabel,
            dataType,
            required,
            dependencies,
            externalType,
            defaultValue,
            hideOption,
            validation,
            options
          ) =>
        FormConfigQuestionResponse(
          id = id,
          label = label.toApi,
          infoLabel = infoLabel.map(_.toApi),
          dataType = dataType.toApi,
          required = required,
          dependencies = dependencies.map(_.toApi),
          visualType = externalType,
          defaultValue = defaultValue,
          hideOption = hideOption.map(_.toApi),
          validation = validation.map(_.toApi),
          options = Some(options.map(_.toApi))
        )
      case MultiQuestion(
            id,
            label,
            infoLabel,
            dataType,
            required,
            dependencies,
            externalType,
            defaultValue,
            hideOption,
            validation,
            options
          ) =>
        FormConfigQuestionResponse(
          id = id,
          label = label.toApi,
          infoLabel = infoLabel.map(_.toApi),
          dataType = dataType.toApi,
          required = required,
          dependencies = dependencies.map(_.toApi),
          visualType = externalType,
          defaultValue = defaultValue,
          hideOption = hideOption.map(_.toApi),
          validation = validation.map(_.toApi),
          options = Some(options.map(_.toApi))
        )
    }
  }

  implicit class LocalizedTextWrapper(private val localizedText: LocalizedText) extends AnyVal {
    def toApi: LocalizedTextResponse =
      LocalizedTextResponse(it = localizedText.it, en = localizedText.en)
  }

  implicit class ValidationWrapper(private val validation: ValidationOption) extends AnyVal {
    def toApi: ValidationOptionResponse =
      ValidationOptionResponse(maxLength = validation.maxLength)
  }

  implicit class MapHideOptionConfigWrapper(private val mapHideOptionConfig: Map[String, Seq[HideOptionConfig]])
      extends AnyVal {
    def toApi: Map[String, Seq[HideOptionResponse]] = mapHideOptionConfig.map { case (k, v) => (k, v.map(_.toApi)) }
  }

  implicit class HideOptionConfigWrapper(private val hideOptionConfig: HideOptionConfig) extends AnyVal {
    def toApi: HideOptionResponse =
      HideOptionResponse(id = hideOptionConfig.id, value = hideOptionConfig.value)
  }

  implicit class LabeledValueWrapper(private val labeledValue: LabeledValue) extends AnyVal {
    def toApi: LabeledValueResponse =
      LabeledValueResponse(label = labeledValue.label.toApi, value = labeledValue.value)
  }

  implicit class DataTypeWrapper(private val dataType: DataType) extends AnyVal {
    def toApi: DataTypeResponse =
      dataType match {
        case Single   => DataTypeResponse.SINGLE
        case Multi    => DataTypeResponse.MULTI
        case FreeText => DataTypeResponse.FREETEXT
      }
  }

  implicit class DependencyWrapper(private val dependency: Dependency) extends AnyVal {
    def toApi: DependencyResponse =
      DependencyResponse(id = dependency.id, value = dependency.value)
  }

  implicit class PurposeVersionSeedWrapper(private val seed: PurposeVersionSeed) extends AnyVal {
    def toManagement: Management.PurposeVersionSeed =
      Management.PurposeVersionSeed(seed.dailyCalls, None)
  }

  implicit class WaitingForApprovalPurposeVersionUpdateContentWrapper(
    private val updateContent: WaitingForApprovalPurposeVersionUpdateContent
  ) extends AnyVal {
    def toManagement: Management.WaitingForApprovalPurposeVersionUpdateContent =
      Management.WaitingForApprovalPurposeVersionUpdateContent(updateContent.expectedApprovalDate)
  }

  implicit class ManagementPurposeVersionWrapper(private val version: Management.PurposeVersion) extends AnyVal {

    def toApi: PurposeVersion                             =
      PurposeVersion(
        id = version.id,
        state = version.state.toApi,
        createdAt = version.createdAt,
        updatedAt = version.updatedAt,
        firstActivationAt = version.firstActivationAt,
        expectedApprovalDate = version.expectedApprovalDate,
        riskAnalysis = version.riskAnalysis.map(_.toApi),
        dailyCalls = version.dailyCalls,
        suspendedAt = version.suspendedAt
      )
    def toPersistent: Persistent.PersistentPurposeVersion =
      Persistent.PersistentPurposeVersion(
        id = version.id,
        state = version.state.toPersistent,
        createdAt = version.createdAt,
        updatedAt = version.updatedAt,
        firstActivationAt = version.firstActivationAt,
        expectedApprovalDate = version.expectedApprovalDate,
        riskAnalysis = version.riskAnalysis.map(_.toPersistent),
        dailyCalls = version.dailyCalls,
        suspendedAt = version.suspendedAt
      )
  }

  implicit class PersistentPurposeVersionWrapper(private val version: Persistent.PersistentPurposeVersion)
      extends AnyVal {
    def toApi: PurposeVersion =
      PurposeVersion(
        id = version.id,
        state = version.state.toApi,
        createdAt = version.createdAt,
        updatedAt = version.updatedAt,
        firstActivationAt = version.firstActivationAt,
        expectedApprovalDate = version.expectedApprovalDate,
        riskAnalysis = version.riskAnalysis.map(_.toApi),
        dailyCalls = version.dailyCalls,
        suspendedAt = version.suspendedAt
      )
  }

  implicit class PurposeVersionStateWrapper(private val state: PurposeVersionState) extends AnyVal {
    def toPersistent: Persistent.PersistentPurposeVersionState =
      state match {
        case PurposeVersionState.ACTIVE               => Persistent.Active
        case PurposeVersionState.DRAFT                => Persistent.Draft
        case PurposeVersionState.SUSPENDED            => Persistent.Suspended
        case PurposeVersionState.WAITING_FOR_APPROVAL => Persistent.WaitingForApproval
        case PurposeVersionState.ARCHIVED             => Persistent.Archived
      }
  }

  implicit class ManagementPurposeVersionStateWrapper(private val state: Management.PurposeVersionState)
      extends AnyVal {
    def toApi: PurposeVersionState =
      state match {
        case Management.PurposeVersionState.ACTIVE               => PurposeVersionState.ACTIVE
        case Management.PurposeVersionState.DRAFT                => PurposeVersionState.DRAFT
        case Management.PurposeVersionState.SUSPENDED            => PurposeVersionState.SUSPENDED
        case Management.PurposeVersionState.WAITING_FOR_APPROVAL => PurposeVersionState.WAITING_FOR_APPROVAL
        case Management.PurposeVersionState.ARCHIVED             => PurposeVersionState.ARCHIVED
      }

    def toPersistent: Persistent.PersistentPurposeVersionState =
      state match {
        case Management.PurposeVersionState.ACTIVE               => Persistent.Active
        case Management.PurposeVersionState.DRAFT                => Persistent.Draft
        case Management.PurposeVersionState.SUSPENDED            => Persistent.Suspended
        case Management.PurposeVersionState.WAITING_FOR_APPROVAL => Persistent.WaitingForApproval
        case Management.PurposeVersionState.ARCHIVED             => Persistent.Archived
      }
  }

  implicit class PersistentPurposeVersionStateWrapper(private val state: Persistent.PersistentPurposeVersionState)
      extends AnyVal {
    def toApi: PurposeVersionState =
      state match {
        case Persistent.Active             => PurposeVersionState.ACTIVE
        case Persistent.Draft              => PurposeVersionState.DRAFT
        case Persistent.Suspended          => PurposeVersionState.SUSPENDED
        case Persistent.WaitingForApproval => PurposeVersionState.WAITING_FOR_APPROVAL
        case Persistent.Archived           => PurposeVersionState.ARCHIVED
      }
  }

  implicit class ManagementPurposeWrapper(private val purpose: Management.Purpose) extends AnyVal {
    def toApi(isRiskAnalysisValid: Boolean): Purpose = Purpose(
      id = purpose.id,
      eserviceId = purpose.eserviceId,
      consumerId = purpose.consumerId,
      versions = purpose.versions.map(_.toApi),
      suspendedByConsumer = purpose.suspendedByConsumer,
      suspendedByProducer = purpose.suspendedByProducer,
      title = purpose.title,
      description = purpose.description,
      riskAnalysisForm = purpose.riskAnalysisForm.map(_.toApi),
      createdAt = purpose.createdAt,
      updatedAt = purpose.updatedAt,
      isRiskAnalysisValid = isRiskAnalysisValid,
      isFreeOfCharge = purpose.isFreeOfCharge,
      freeOfChargeReason = purpose.freeOfChargeReason
    )
  }

  implicit class PersistentPurposeWrapper(private val purpose: Persistent.PersistentPurpose) extends AnyVal {
    def toApi(isRiskAnalysisValid: Boolean): Purpose = Purpose(
      id = purpose.id,
      eserviceId = purpose.eserviceId,
      consumerId = purpose.consumerId,
      versions = purpose.versions.map(_.toApi),
      suspendedByConsumer = purpose.suspendedByConsumer,
      suspendedByProducer = purpose.suspendedByProducer,
      title = purpose.title,
      description = purpose.description,
      riskAnalysisForm = purpose.riskAnalysisForm.map(_.toApi),
      createdAt = purpose.createdAt,
      updatedAt = purpose.updatedAt,
      isRiskAnalysisValid = isRiskAnalysisValid,
      isFreeOfCharge = purpose.isFreeOfCharge,
      freeOfChargeReason = purpose.freeOfChargeReason
    )
  }

  implicit class PurposeUpdateContentWrapper(private val content: PurposeUpdateContent) extends AnyVal {
    def toManagement(
      schemaOnlyValidation: Boolean
    )(kind: PersistentTenantKind): Either[Throwable, Management.PurposeUpdateContent] = {
      for {
        riskAnalysisForm <- content.riskAnalysisForm
          .traverse(risk =>
            RiskAnalysisValidation
              .validate(risk.toTemplate, schemaOnlyValidation = schemaOnlyValidation)(kind.toTemplate)
              .leftMap(RiskAnalysisValidationFailed(_))
              .toEither
              .map(_.toManagement(risk.riskAnalysisId))
          )
      } yield Management.PurposeUpdateContent(
        title = content.title,
        description = content.description,
        isFreeOfCharge = content.isFreeOfCharge,
        freeOfChargeReason = content.freeOfChargeReason,
        riskAnalysisForm = riskAnalysisForm,
        dailyCalls = content.dailyCalls
      )
    }
  }

  implicit class ReversePurposeUpdateContentWrapper(private val content: ReversePurposeUpdateContent) extends AnyVal {
    def toManagement(schemaOnlyValidation: Boolean, originalRiskAnalysisForm: Option[RiskAnalysisForm])(
      kind: PersistentTenantKind
    ): Either[Throwable, Management.PurposeUpdateContent] = {
      for {
        riskAnalysisForm <- originalRiskAnalysisForm
          .traverse(risk =>
            RiskAnalysisValidation
              .validate(risk.toTemplate, schemaOnlyValidation = schemaOnlyValidation)(kind.toTemplate)
              .leftMap(RiskAnalysisValidationFailed(_))
              .toEither
              .map(_.toManagement(risk.riskAnalysisId))
          )
      } yield Management.PurposeUpdateContent(
        title = content.title,
        description = content.description,
        isFreeOfCharge = content.isFreeOfCharge,
        freeOfChargeReason = content.freeOfChargeReason,
        riskAnalysisForm = riskAnalysisForm,
        dailyCalls = content.dailyCalls
      )
    }
  }
}
