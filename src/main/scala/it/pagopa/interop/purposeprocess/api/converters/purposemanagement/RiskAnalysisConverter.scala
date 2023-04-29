package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysis,
  RiskAnalysisMultiAnswer => DepRiskAnalysisMultiAnswer,
  RiskAnalysisSingleAnswer => DepRiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposemanagement.model.purpose.{
  PersistentRiskAnalysisForm,
  PersistentRiskAnalysisMultiAnswer,
  PersistentRiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.model.{
  RiskAnalysisForm,
  RiskAnalysisFormConfigResponse,
  FormConfigQuestionResponse,
  LocalizedTextResponse,
  DataTypeResponse,
  DependencyResponse,
  LabeledValueResponse
}
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate.{
  RiskAnalysisFormConfig,
  FormConfigQuestion,
  LocalizedText,
  DataType,
  Single,
  Multi,
  FreeText,
  Dependency,
  LabeledValue,
  FreeInputQuestion,
  SingleQuestion,
  MultiQuestion
}

object RiskAnalysisConverter {
  def dependencyToApi(riskAnalysis: DepRiskAnalysis): RiskAnalysisForm =
    RiskAnalysisForm(
      version = riskAnalysis.version,
      answers = singleAnswersToApi(riskAnalysis.singleAnswers) ++ multiAnswersToApi(riskAnalysis.multiAnswers)
    )

  def singleAnswersToApi(singleAnswers: Seq[DepRiskAnalysisSingleAnswer]): Map[String, Seq[String]] =
    singleAnswers.map(a => (a.key, a.value.toSeq)).toMap

  def multiAnswersToApi(multiAnswers: Seq[DepRiskAnalysisMultiAnswer]): Map[String, Seq[String]] =
    multiAnswers.map(a => (a.key, a.values)).toMap

  def persistentToApi(riskAnalysis: PersistentRiskAnalysisForm): RiskAnalysisForm =
    RiskAnalysisForm(
      version = riskAnalysis.version,
      answers = persistentSingleAnswersToApi(riskAnalysis.singleAnswers) ++ persistentMultiAnswersToApi(
        riskAnalysis.multiAnswers
      )
    )

  def persistentSingleAnswersToApi(singleAnswers: Seq[PersistentRiskAnalysisSingleAnswer]): Map[String, Seq[String]] =
    singleAnswers.map(a => (a.key, a.value.toSeq)).toMap

  def persistentMultiAnswersToApi(multiAnswers: Seq[PersistentRiskAnalysisMultiAnswer]): Map[String, Seq[String]] =
    multiAnswers.map(a => (a.key, a.values)).toMap

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
      case FreeInputQuestion(id, label, infoLabel, dataType, required, dependencies, externalType)       =>
        FormConfigQuestionResponse(
          id = id,
          label = label.toApi,
          infoLabel = infoLabel.map(_.toApi),
          dataType = dataType.toApi,
          required = required,
          dependencies = dependencies.map(_.toApi),
          visualType = externalType
        )
      case SingleQuestion(id, label, infoLabel, dataType, required, dependencies, externalType, options) =>
        FormConfigQuestionResponse(
          id = id,
          label = label.toApi,
          infoLabel = infoLabel.map(_.toApi),
          dataType = dataType.toApi,
          required = required,
          dependencies = dependencies.map(_.toApi),
          visualType = externalType,
          options = Some(options.map(_.toApi))
        )
      case MultiQuestion(id, label, infoLabel, dataType, required, dependencies, externalType, options)  =>
        FormConfigQuestionResponse(
          id = id,
          label = label.toApi,
          infoLabel = infoLabel.map(_.toApi),
          dataType = dataType.toApi,
          required = required,
          dependencies = dependencies.map(_.toApi),
          visualType = externalType,
          options = Some(options.map(_.toApi))
        )
    }
  }

  implicit class LocalizedTextWrapper(private val localizedText: LocalizedText) extends AnyVal {
    def toApi: LocalizedTextResponse =
      LocalizedTextResponse(it = localizedText.it, en = localizedText.en)
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
}
