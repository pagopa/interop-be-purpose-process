package it.pagopa.interop.purposeprocess.api.converters.purposemanagement

import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysis,
  RiskAnalysisMultiAnswer => DepRiskAnalysisMultiAnswer,
  RiskAnalysisSingleAnswer => DepRiskAnalysisSingleAnswer
}
import it.pagopa.interop.purposeprocess.api.impl.ValidationRules
import it.pagopa.interop.purposeprocess.model.{
  FormDataQuantityAnswers,
  FormDeliveryMethodAnswers,
  FormLegalBasisAnswers,
  FormPurposePursuitAnswers,
  RiskAnalysisForm,
  RiskAnalysisFormAnswers,
  RiskAnalysisFormYesAnswer,
  RiskAnalysisFormYesNoAnswer
}
import cats.implicits._

object RiskAnalysisConverter {
  def dependencyToApi(riskAnalysis: DepRiskAnalysis): Either[Throwable, RiskAnalysisForm] = {
    for {
      answers <- answersToApi(riskAnalysis.singleAnswers, riskAnalysis.multiAnswers)
    } yield RiskAnalysisForm(version = riskAnalysis.version, answers = answers)
  }

  def answersToApi(
    singleAnswers: Seq[DepRiskAnalysisSingleAnswer],
    multiAnswers: Seq[DepRiskAnalysisMultiAnswer]
  ): Either[Throwable, RiskAnalysisFormAnswers] = {
    for {
      purposeOpt          <- toSingleAnswer(singleAnswers, ValidationRules.PURPOSE, Right(_))
      purpose             <- purposeOpt.toRight(new RuntimeException(""))
      usesPersonalDataOpt <- toYesNoSingleAnswer(singleAnswers, ValidationRules.USES_PERSONAL_DATA)
      usesPersonalData    <- usesPersonalDataOpt.toRight(new RuntimeException(""))

      usesThirdPartyPersonalData  <- toYesNoSingleAnswer(singleAnswers, ValidationRules.USES_THIRD_PARTY_PERSONAL_DATA)
      usesConfidentialData        <- toYesNoSingleAnswer(singleAnswers, ValidationRules.USES_CONFIDENTIAL_DATA)
      securedDataAccess           <- toYesNoSingleAnswer(singleAnswers, ValidationRules.SECURED_DATA_ACCESS)
      legalObligationReference    <- toSingleAnswer(singleAnswers, ValidationRules.LEGAL_OBLIGATION_REFERENCE, Right(_))
      publicInterestReference     <- toSingleAnswer(singleAnswers, ValidationRules.PUBLIC_INTEREST_REFERENCE, Right(_))
      knowsAccessedDataCategories <- toYesNoSingleAnswer(singleAnswers, ValidationRules.KNOWS_ACCESSED_DATA_CATEGORIES)
      accessDataArt9Gdpr          <- toYesNoSingleAnswer(singleAnswers, ValidationRules.ACCESS_DATA_ART9_GDPR)
      accessUnderageData          <- toYesNoSingleAnswer(singleAnswers, ValidationRules.ACCESS_UNDERAGE_DATA)
      knowsDataQuantity           <- toYesNoSingleAnswer(singleAnswers, ValidationRules.KNOWS_DATA_QUANTITY)
      dataQuantity                <- toSingleAnswer(singleAnswers, ValidationRules.DATA_QUANTITY, FormDataQuantityAnswers.fromValue)
      deliveryMethod <- toSingleAnswer(
        singleAnswers,
        ValidationRules.DELIVERY_METHOD,
        FormDeliveryMethodAnswers.fromValue
      )
      doneDpia                   <- toYesNoSingleAnswer(singleAnswers, ValidationRules.DONE_DPIA)
      definedDataRetentionPeriod <- toYesNoSingleAnswer(singleAnswers, ValidationRules.DEFINED_DATA_RETENTION_PERIOD)
      purposePursuit <- toSingleAnswer(
        singleAnswers,
        ValidationRules.PURPOSE_PURSUIT,
        FormPurposePursuitAnswers.fromValue
      )
      checkedAllDataNeeded <- toYesNoSingleAnswer(singleAnswers, ValidationRules.CHECKED_ALL_DATA_NEEDED)
      minimalDataInteropCatalogue <- toYesNoSingleAnswer(
        singleAnswers,
        ValidationRules.CHECKED_EXISTENCE_MINIMAL_DATA_INTEROP_CATALOGUE
      )
      legalBasis <- toMultiAnswer(multiAnswers, ValidationRules.LEGAL_BASIS, FormLegalBasisAnswers.fromValue)
      mereCorrectnessInteropCatalogue <- toMultiAnswer(
        multiAnswers,
        ValidationRules.CHECKED_EXISTENCE_MERE_CORRECTNESS_INTEROP_CATALOGUE,
        RiskAnalysisFormYesAnswer.fromValue
      )
    } yield RiskAnalysisFormAnswers(
      purpose = purpose,
      usesPersonalData = usesPersonalData,
      usesThirdPartyPersonalData = usesThirdPartyPersonalData,
      usesConfidentialData = usesConfidentialData,
      securedDataAccess = securedDataAccess,
      legalBasis = legalBasis,
      legalObligationReference = legalObligationReference,
      publicInterestReference = publicInterestReference,
      knowsAccessedDataCategories = knowsAccessedDataCategories,
      accessDataArt9Gdpr = accessDataArt9Gdpr,
      accessUnderageData = accessUnderageData,
      knowsDataQuantity = knowsDataQuantity,
      dataQuantity = dataQuantity,
      deliveryMethod = deliveryMethod,
      doneDpia = doneDpia,
      definedDataRetentionPeriod = definedDataRetentionPeriod,
      purposePursuit = purposePursuit,
      checkedExistenceMereCorrectnessInteropCatalogue = mereCorrectnessInteropCatalogue,
      checkedAllDataNeeded = checkedAllDataNeeded,
      checkedExistenceMinimalDataInteropCatalogue = minimalDataInteropCatalogue
    )
  }

  def toYesNoSingleAnswer(
    singleAnswers: Seq[DepRiskAnalysisSingleAnswer],
    fieldName: String
  ): Either[Throwable, Option[RiskAnalysisFormYesNoAnswer]] =
    toSingleAnswer(singleAnswers, fieldName, RiskAnalysisFormYesNoAnswer.fromValue)

  def toSingleAnswer[T](
    singleAnswers: Seq[DepRiskAnalysisSingleAnswer],
    fieldName: String,
    f: String => Either[Throwable, T]
  ): Either[Throwable, Option[T]] =
    singleAnswers.find(_.key == fieldName).flatTraverse(_.value.traverse(f))

  def toMultiAnswer[T](
    multiAnswers: Seq[DepRiskAnalysisMultiAnswer],
    fieldName: String,
    f: String => Either[Throwable, T]
  ): Either[Throwable, Option[Seq[T]]] =
    multiAnswers
      .find(_.key == fieldName)
      .traverse(_.values.traverse(f))
}
