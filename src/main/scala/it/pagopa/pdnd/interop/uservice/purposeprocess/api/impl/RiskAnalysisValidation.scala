package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import cats.data._
import cats.implicits._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  RiskAnalysisFormSeed => RiskAnalysisFormSeed,
  RiskAnalysisMultiAnswerSeed => MultiAnswerSeed,
  RiskAnalysisSingleAnswerSeed => SingleAnswerSeed
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import spray.json._

object RiskAnalysisValidation {
  import ValidationRules._

  type ValidationResult[A] = ValidatedNec[RiskAnalysisValidationError, A]

  /** Validate a Process risk analysis form and returns the same in the Management format
    * @param form Risk Analysis Form
    * @return Validated risk analysis
    */
  def validate(form: RiskAnalysisForm): ValidationResult[RiskAnalysisFormSeed] = {
    val answersJson: JsObject = form.answers.toJson.asJsObject

    val validations = validateForm(answersJson, validationRules)

    val singleAnswers: ValidationResult[Seq[SingleAnswerSeed]] = validations.collect { case Left(s) => s }.sequence
    val multiAnswers: ValidationResult[Seq[MultiAnswerSeed]]   = validations.collect { case Right(m) => m }.sequence
    val expectedFields: ValidationResult[List[Unit]]           = validateExpectedFields(answersJson, validationRules)

    (singleAnswers, multiAnswers, expectedFields).mapN((l1, l2, _) =>
      RiskAnalysisFormSeed(version = form.version, singleAnswers = l1, multiAnswers = l2)
    )
  }

  /** Verify that each field is present when the related dependencies are met
    * @param answersJson form in json format
    * @param validationRules validation rules
    * @return
    */
  def validateExpectedFields(
    answersJson: JsObject,
    validationRules: List[ValidationEntry]
  ): ValidationResult[List[Unit]] = {
    validationRules
      .filter(_.required)
      .map { entry =>
        val dependenciesSatisfied: Boolean = entry.dependencies.forall(formContainsDependency(answersJson, _))

        if (
          !dependenciesSatisfied ||
          (dependenciesSatisfied && answersJson.getFields(entry.fieldName).nonEmpty)
        )
          ().validNec
        else
          MissingExpectedField(entry.fieldName).invalidNec
      }
      .sequence
  }

  /** Check if the form contains the dependency (field with the specific value)
    * @param answersJson form in json format
    * @param dependency dependency
    * @return true if contained, false otherwise
    */
  def formContainsDependency(answersJson: JsObject, dependency: DependencyEntry): Boolean =
    answersJson.getFields(dependency.fieldName).exists { f =>
      f match {
        case str: JsString => str.value == dependency.fieldValue
        case arr: JsArray  => jsArrayContains(arr, dependency.fieldValue)
        case _             => false
      }
    }

  /** Validate the form using the validation rules
    * @param answersJson form in json format
    * @param validationRules validation rules
    * @return List of validations for single and multiple answers
    */
  def validateForm(
    answersJson: JsObject,
    validationRules: List[ValidationEntry]
  ): Seq[Either[ValidationResult[SingleAnswerSeed], ValidationResult[MultiAnswerSeed]]] = {
    answersJson.fields.map { case (key, value) =>
      validateField(answersJson, validationRules)(key).fold(
        err => Left[ValidationResult[SingleAnswerSeed], Nothing](err.invalid),
        _ => answerToDependency(key, value)
      )
    }.toSeq
  }

  /** Convert a form answer to a Management answer
    * @param fieldName form field name
    * @param value form field value
    * @return Either for the validation of the SingleAnswer (Left) or MultiAnswer (Right)
    */
  def answerToDependency(
    fieldName: String,
    value: JsValue
  ): Either[ValidationResult[SingleAnswerSeed], ValidationResult[MultiAnswerSeed]] =
    value match {
      case str: JsString =>
        Left(SingleAnswerSeed(fieldName, Some(str.value)).validNec[RiskAnalysisValidationError])
      case arr: JsArray =>
        val values: Seq[ValidationResult[String]] = {
          arr.elements.map {
            case str: JsString => str.value.validNec
            case _             => UnexpectedFieldFormat(fieldName).invalidNec
          }
        }
        Right(values.sequence.map(MultiAnswerSeed(fieldName, _)))
      case _ =>
        Left(UnexpectedFieldFormat(fieldName).invalidNec)
    }

  /** Verify that the field found in the form satisfies the validation rules
    * @param answersJson form in json format
    * @param validationRules validation rules
    * @param fieldName field name
    * @return
    */
  def validateField(answersJson: JsObject, validationRules: List[ValidationEntry])(
    fieldName: String
  ): ValidationResult[Seq[Unit]] =
    validationRules.find(_.fieldName == fieldName) match {
      case Some(rule) =>
        rule.dependencies.map(validateDependency(answersJson: JsObject)).sequence
      case None =>
        UnexpectedField(fieldName).invalidNec
    }

  /** Verify that the form contains the dependency
    * @param answersJson form in json format
    * @param dependency dependency
    * @return Validation result
    */
  def validateDependency(answersJson: JsObject)(dependency: DependencyEntry): ValidationResult[Unit] =
    answersJson.getFields(dependency.fieldName) match {
      case Nil =>
        DependencyNotFound(dependency.fieldName).invalidNec
      case f :: Nil =>
        f match {
          case str: JsString if str.value == dependency.fieldValue =>
            ().validNec
          case arr: JsArray if jsArrayContains(arr, dependency.fieldValue) =>
            ().validNec
          case _ =>
            UnexpectedFieldValue(dependency.fieldName).invalidNec
        }
      case _ :: _ =>
        TooManyOccurrences(dependency.fieldName).invalidNec
    }

  /** Check if a JsArray contains a specific String (JsString)
    * @param arr JsArray
    * @param value the string
    * @return true if array contains the string, false otherwise
    */
  def jsArrayContains(arr: JsArray, value: String): Boolean =
    arr.elements.collectFirst { case v: JsString if v.value == value => () }.nonEmpty

}

object ValidationRules {

  final case class DependencyEntry(fieldName: String, fieldValue: String)
  final case class ValidationEntry(fieldName: String, required: Boolean, dependencies: Seq[DependencyEntry])

  val YES: String = RiskAnalysisFormYesNoAnswer.YES.toString
  val NO: String  = RiskAnalysisFormYesNoAnswer.NO.toString

  // Fields names
  val PURPOSE: String                                              = "purpose"
  val ACCESS_DATA_ART9_GDPR: String                                = "accessDataArt9Gdpr"
  val ACCESS_UNDERAGE_DATA: String                                 = "accessUnderageData"
  val CHECKED_ALL_DATA_NEEDED: String                              = "checkedAllDataNeeded"
  val CHECKED_EXISTENCE_MERE_CORRECTNESS_INTEROP_CATALOGUE: String = "checkedExistenceMereCorrectnessInteropCatalogue"
  val CHECKED_EXISTENCE_MINIMAL_DATA_INTEROP_CATALOGUE: String     = "checkedExistenceMinimalDataInteropCatalogue"
  val DATA_QUANTITY: String                                        = "dataQuantity"
  val DEFINED_DATA_RETENTION_PERIOD: String                        = "definedDataRetentionPeriod"
  val DELIVERY_METHOD: String                                      = "deliveryMethod"
  val DONE_DPIA: String                                            = "doneDpia"
  val KNOWS_ACCESSED_DATA_CATEGORIES: String                       = "knowsAccessedDataCategories"
  val KNOWS_DATA_QUANTITY: String                                  = "knowsDataQuantity"
  val LEGAL_BASIS: String                                          = "legalBasis"
  val LEGAL_OBLIGATION_REFERENCE: String                           = "legalObligationReference"
  val PUBLIC_INTEREST_REFERENCE: String                            = "publicInterestReference"
  val PURPOSE_PURSUIT: String                                      = "purposePursuit"
  val SECURED_DATA_ACCESS: String                                  = "securedDataAccess"
  val USES_CONFIDENTIAL_DATA: String                               = "usesConfidentialData"
  val USES_PERSONAL_DATA: String                                   = "usesPersonalData"
  val USES_THIRD_PARTY_PERSONAL_DATA: String                       = "usesThirdPartyPersonalData"
  // End Fields names

  val validationRules: List[ValidationEntry] = List(
    ValidationEntry(PURPOSE, required = true, Seq.empty),
    ValidationEntry(USES_PERSONAL_DATA, required = true, Seq.empty),
    ValidationEntry(USES_THIRD_PARTY_PERSONAL_DATA, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, NO))),
    ValidationEntry(
      USES_CONFIDENTIAL_DATA,
      required = true,
      Seq(DependencyEntry(USES_PERSONAL_DATA, NO), DependencyEntry(USES_THIRD_PARTY_PERSONAL_DATA, YES))
    ),
    ValidationEntry(SECURED_DATA_ACCESS, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, NO))),
    ValidationEntry(LEGAL_BASIS, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, YES))),
    ValidationEntry(
      LEGAL_OBLIGATION_REFERENCE,
      required = true,
      Seq(DependencyEntry(LEGAL_BASIS, FormLegalBasisAnswers.LEGAL_OBLIGATION.toString))
    ),
    ValidationEntry(
      PUBLIC_INTEREST_REFERENCE,
      required = true,
      Seq(DependencyEntry(LEGAL_BASIS, FormLegalBasisAnswers.PUBLIC_INTEREST.toString))
    ),
    ValidationEntry(KNOWS_ACCESSED_DATA_CATEGORIES, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, YES))),
    ValidationEntry(
      ACCESS_DATA_ART9_GDPR,
      required = true,
      Seq(DependencyEntry(USES_PERSONAL_DATA, YES), DependencyEntry(KNOWS_ACCESSED_DATA_CATEGORIES, YES))
    ),
    ValidationEntry(
      ACCESS_UNDERAGE_DATA,
      required = true,
      Seq(DependencyEntry(USES_PERSONAL_DATA, YES), DependencyEntry(KNOWS_ACCESSED_DATA_CATEGORIES, YES))
    ),
    ValidationEntry(KNOWS_DATA_QUANTITY, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, YES))),
    ValidationEntry(
      DATA_QUANTITY,
      required = true,
      Seq(DependencyEntry(USES_PERSONAL_DATA, YES), DependencyEntry(KNOWS_DATA_QUANTITY, YES))
    ),
    ValidationEntry(DELIVERY_METHOD, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, YES))),
    ValidationEntry(DONE_DPIA, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, YES))),
    ValidationEntry(DEFINED_DATA_RETENTION_PERIOD, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, YES))),
    ValidationEntry(PURPOSE_PURSUIT, required = true, Seq(DependencyEntry(USES_PERSONAL_DATA, YES))),
    ValidationEntry(
      CHECKED_EXISTENCE_MERE_CORRECTNESS_INTEROP_CATALOGUE,
      required = true,
      Seq(
        DependencyEntry(USES_PERSONAL_DATA, YES),
        DependencyEntry(PURPOSE_PURSUIT, FormPurposePursuitAnswers.MERE_CORRECTNESS.toString)
      )
    ),
    ValidationEntry(
      CHECKED_ALL_DATA_NEEDED,
      required = true,
      Seq(
        DependencyEntry(USES_PERSONAL_DATA, YES),
        DependencyEntry(PURPOSE_PURSUIT, FormPurposePursuitAnswers.NEW_PERSONAL_DATA.toString)
      )
    ),
    ValidationEntry(
      CHECKED_EXISTENCE_MINIMAL_DATA_INTEROP_CATALOGUE,
      required = true,
      Seq(
        DependencyEntry(USES_PERSONAL_DATA, YES),
        DependencyEntry(PURPOSE_PURSUIT, FormPurposePursuitAnswers.NEW_PERSONAL_DATA.toString),
        DependencyEntry(CHECKED_ALL_DATA_NEEDED, NO)
      )
    )
  )
}
