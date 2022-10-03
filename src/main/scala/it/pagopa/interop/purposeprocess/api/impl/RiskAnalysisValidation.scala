package it.pagopa.interop.purposeprocess.api.impl

import cats.data._
import cats.implicits._
import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisFormSeed,
  RiskAnalysisMultiAnswerSeed => MultiAnswerSeed,
  RiskAnalysisSingleAnswerSeed => SingleAnswerSeed
}
import it.pagopa.interop.purposeprocess.error._
import it.pagopa.interop.purposeprocess.model._
import it.pagopa.interop.purposeprocess.model.riskAnalysisTemplate._
import spray.json._

import scala.io.Source

object RiskAnalysisValidation {

  type ValidationResult[A] = ValidatedNec[RiskAnalysisValidationError, A]

  /** Validate a Process risk analysis form and returns the same in the Management format
    * @param form Risk Analysis Form
    * @return Validated risk analysis
    */
  def validate(form: RiskAnalysisForm): ValidationResult[RiskAnalysisFormSeed] = {

    val configs1_0 = loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/1.0.json")
    val configs2_0 = loadRiskAnalysisFormConfig("riskAnalysisTemplate/forms/2.0.json")

    val sanitizedForm = form.copy(answers = form.answers.filter(_._2.nonEmpty))
    // TODO Test this
    val validationRules: ValidationResult[List[ValidationEntry]] = sanitizedForm.version match {
      // TODO Enum?
      case "1.0" => configsToRules(configs1_0).validNec
      case "2.0" => configsToRules(configs2_0).validNec
      case other => UnexpectedTemplateVersion(other).invalidNec
    }

    validationRules.andThen(validateFormWithRules(_, sanitizedForm))

  }

  def validateFormWithRules(
    validationRules: List[ValidationEntry],
    form: RiskAnalysisForm
  ): ValidationResult[RiskAnalysisFormSeed] = {
    val answersJson: JsObject = form.answers.toJson.asJsObject
    val validations           = validateForm(answersJson, validationRules)

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
      validateField(answersJson, validationRules)(key, value).fold(
        err => Left[ValidationResult[SingleAnswerSeed], Nothing](err.invalid),
        rule => answerToDependency(rule, key, value)
      )
    }.toSeq
  }

  /** Convert a form answer to a Management answer
    * @param fieldName form field name
    * @param value form field value
    * @return Either for the validation of the SingleAnswer (Left) or MultiAnswer (Right)
    */
  def answerToDependency(
    rule: ValidationEntry,
    fieldName: String,
    value: JsValue
  ): Either[ValidationResult[SingleAnswerSeed], ValidationResult[MultiAnswerSeed]] =
    value match {
//      case str: JsString =>
//        Left(SingleAnswerSeed(fieldName, Some(str.value)).validNec[RiskAnalysisValidationError])
      case arr: JsArray if rule.dataType == "single" || rule.dataType == "freeText" =>
        val value: ValidationResult[String] = {
          arr.elements.headOption match {
            case Some(str: JsString) => str.value.validNec
            case _                   => UnexpectedFieldFormat(fieldName).invalidNec
          }
        }
        Left(value.map(v => SingleAnswerSeed(fieldName, v.some)))
      case arr: JsArray if rule.dataType == "multi"                                 =>
        val values: Seq[ValidationResult[String]] = {
          arr.elements.map {
            case str: JsString => str.value.validNec
            case _             => UnexpectedFieldFormat(fieldName).invalidNec
          }
        }
        Right(values.sequence.map(MultiAnswerSeed(fieldName, _)))
      case _                                                                        =>
        Left(UnexpectedFieldFormat(fieldName).invalidNec)
    }

  /** Verify that the field found in the form satisfies the validation rules
    * @param answersJson form in json format
    * @param validationRules validation rules
    * @param fieldName field name
    * @return
    */
  def validateField(
    answersJson: JsObject,
    validationRules: List[ValidationEntry]
  )(fieldName: String, value: JsValue): ValidationResult[ValidationEntry] =
    validationRules.find(_.fieldName == fieldName) match {
      case Some(rule) =>
        validateAllowedValue(rule, value)
          .andThen(_ => rule.dependencies.map(validateDependency(answersJson, rule.fieldName)).sequence)
          .as(rule)
      case None       =>
        UnexpectedField(fieldName).invalidNec
    }

  /** Verify that the form contains the dependency
    * @param answersJson form in json format
    * @param dependency dependency
    * @return Validation result
    */
  def validateDependency(answersJson: JsObject, dependentField: String)(
    dependency: DependencyEntry
  ): ValidationResult[Unit] =
    answersJson.getFields(dependency.fieldName) match {
      case Nil      =>
        DependencyNotFound(dependency.fieldName, dependentField).invalidNec
      case f :: Nil =>
        f match {
          case str: JsString if str.value == dependency.fieldValue         =>
            ().validNec
          case arr: JsArray if jsArrayContains(arr, dependency.fieldValue) =>
            ().validNec
          case _                                                           =>
            UnexpectedFieldValueByDependency(dependency.fieldName, dependentField, dependency.fieldValue).invalidNec
        }
      case _        => TooManyOccurrences(dependency.fieldName).invalidNec
    }

  def validateAllowedValue(rule: ValidationEntry, value: JsValue): ValidationResult[Unit] =
    value match {
      case str: JsString =>
        if (rule.allowedValues.forall(_.contains(str.value))) ().validNec
        else UnexpectedFieldValue(rule.fieldName, rule.allowedValues).invalidNec
      case arr: JsArray  =>
        rule.allowedValues.fold[ValidationResult[Unit]](().validNec)(jsArrayIsSubset(rule.fieldName, arr, _))
      case _             =>
        UnexpectedFieldValue(rule.fieldName, rule.allowedValues).invalidNec
    }

  /** Check if a JsArray contains a specific String (JsString)
    * @param arr JsArray
    * @param value the string
    * @return true if array contains the string, false otherwise
    */
  def jsArrayContains(arr: JsArray, value: String): Boolean =
    arr.elements.collectFirst { case v: JsString if v.value == value => () }.nonEmpty

  def jsArrayIsSubset(fieldName: String, arr: JsArray, values: Set[String]): ValidationResult[Unit] =
    arr.elements
      .map {
        case v: JsString if values.contains(v.value) => ().validNec
        case _                                       => UnexpectedFieldValue(fieldName, values.some).invalidNec
      }
      .sequence
      .map(_ => ())

  private[this] def loadRiskAnalysisFormConfig(resourcePath: String) =
    Source
      .fromResource(resourcePath)
      .getLines()
      .mkString(System.lineSeparator())
      .parseJson
      .convertTo[RiskAnalysisFormConfig]

  def dependencyConfigToRule(dependency: Dependency): DependencyEntry =
    DependencyEntry(fieldName = dependency.id, fieldValue = dependency.value)

  def questionToValidationEntry(question: FormConfigQuestion): ValidationEntry =
    question match {
      case c: FreeInputQuestion =>
        ValidationEntry(
          fieldName = c.id,
          dataType = c.dataType,
          required = c.required,
          dependencies = c.dependencies.map(dependencyConfigToRule),
          allowedValues = None
        )
      case c: SingleQuestion    =>
        ValidationEntry(
          fieldName = c.id,
          dataType = c.dataType,
          required = c.required,
          dependencies = c.dependencies.map(dependencyConfigToRule),
          allowedValues = c.options.map(_.value).toSet.some
        )
      case c: MultiQuestion     =>
        ValidationEntry(
          fieldName = c.id,
          dataType = c.dataType,
          required = c.required,
          dependencies = c.dependencies.map(dependencyConfigToRule),
          allowedValues = c.options.map(_.value).toSet.some
        )
    }

  def configsToRules(config: RiskAnalysisFormConfig): List[ValidationEntry] =
    config.questions.map(questionToValidationEntry)

}

final case class DependencyEntry(fieldName: String, fieldValue: String)
final case class ValidationEntry(
  fieldName: String,
  dataType: String, // TODO Enum?
  required: Boolean,
  dependencies: Seq[DependencyEntry],
  allowedValues: Option[Set[String]]
)

//// TODO Move to different file
//object ValidationRulesV1 {
//
//  // Answers
//  object YesNoAnswer extends Enumeration {
//    type YesNoAnswer = Value
//    val YES, NO                = Value
//    val valuesSet: Set[String] = values.map(_.toString)
//  }
//
//  val YES: String = YesNoAnswer.YES.toString
//  val NO: String  = YesNoAnswer.NO.toString
//
//  object LegalBasisAnswer extends Enumeration {
//    type FormLegalBasisAnswers = Value
//    val CONSENT, CONTRACT, LEGAL_OBLIGATION, SAFEGUARD, PUBLIC_INTEREST, LEGITIMATE_INTEREST = Value
//    val valuesSet: Set[String]                                                               = values.map(_.toString)
//  }
//
//  object DataQuantityAnswer extends Enumeration {
//    type FormDataQuantityAnswers = Value
//    val QUANTITY_0_TO_100, QUANTITY_101_TO_500, QUANTITY_500_TO_1000, QUANTITY_1001_TO_5000, QUANTITY_5001_OVER = Value
//    val valuesSet: Set[String] = values.map(_.toString)
//  }
//
//  object DeliveryMethodAnswer extends Enumeration {
//    type FormDeliveryMethodAnswers = Value
//    val CLEARTEXT, AGGREGATE, ANONYMOUS, PSEUDOANONYMOUS = Value
//    val valuesSet: Set[String]                           = values.map(_.toString)
//  }
//
//  object PurposePursuitAnswer extends Enumeration {
//    type FormPurposePursuitAnswers = Value
//    val MERE_CORRECTNESS, NEW_PERSONAL_DATA = Value
//    val valuesSet: Set[String]              = values.map(_.toString)
//  }
//  // End Answers
//
//  // Fields names
//  val PURPOSE: String                                              = "purpose"
//  val ACCESS_DATA_ART9_GDPR: String                                = "accessDataArt9Gdpr"
//  val ACCESS_UNDERAGE_DATA: String                                 = "accessUnderageData"
//  val CHECKED_ALL_DATA_NEEDED: String                              = "checkedAllDataNeeded"
//  val CHECKED_EXISTENCE_MERE_CORRECTNESS_INTEROP_CATALOGUE: String = "checkedExistenceMereCorrectnessInteropCatalogue"
//  val CHECKED_EXISTENCE_MINIMAL_DATA_INTEROP_CATALOGUE: String     = "checkedExistenceMinimalDataInteropCatalogue"
//  val DATA_QUANTITY: String                                        = "dataQuantity"
//  val DEFINED_DATA_RETENTION_PERIOD: String                        = "definedDataRetentionPeriod"
//  val DELIVERY_METHOD: String                                      = "deliveryMethod"
//  val DONE_DPIA: String                                            = "doneDpia"
//  val KNOWS_ACCESSED_DATA_CATEGORIES: String                       = "knowsAccessedDataCategories"
//  val KNOWS_DATA_QUANTITY: String                                  = "knowsDataQuantity"
//  val LEGAL_BASIS: String                                          = "legalBasis"
//  val LEGAL_OBLIGATION_REFERENCE: String                           = "legalObligationReference"
//  val PUBLIC_INTEREST_REFERENCE: String                            = "publicInterestReference"
//  val PURPOSE_PURSUIT: String                                      = "purposePursuit"
//  val SECURED_DATA_ACCESS: String                                  = "securedDataAccess"
//  val USES_CONFIDENTIAL_DATA: String                               = "usesConfidentialData"
//  val USES_PERSONAL_DATA: String                                   = "usesPersonalData"
//  val USES_THIRD_PARTY_PERSONAL_DATA: String                       = "usesThirdPartyPersonalData"
//  // End Fields names
//
//  val validationRules: List[ValidationEntry] = List(
//    ValidationEntry(PURPOSE, required = true, Seq.empty, None),
//    ValidationEntry(USES_PERSONAL_DATA, required = true, Seq.empty, YesNoAnswer.valuesSet.some),
//    ValidationEntry(
//      USES_THIRD_PARTY_PERSONAL_DATA,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, NO)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      USES_CONFIDENTIAL_DATA,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, NO), DependencyEntry(USES_THIRD_PARTY_PERSONAL_DATA, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      SECURED_DATA_ACCESS,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, NO)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      LEGAL_BASIS,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      LEGAL_OBLIGATION_REFERENCE,
//      required = true,
//      Seq(DependencyEntry(LEGAL_BASIS, LegalBasisAnswer.LEGAL_OBLIGATION.toString)),
//      None
//    ),
//    ValidationEntry(
//      PUBLIC_INTEREST_REFERENCE,
//      required = true,
//      Seq(DependencyEntry(LEGAL_BASIS, LegalBasisAnswer.PUBLIC_INTEREST.toString)),
//      None
//    ),
//    ValidationEntry(
//      KNOWS_ACCESSED_DATA_CATEGORIES,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      ACCESS_DATA_ART9_GDPR,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES), DependencyEntry(KNOWS_ACCESSED_DATA_CATEGORIES, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      ACCESS_UNDERAGE_DATA,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES), DependencyEntry(KNOWS_ACCESSED_DATA_CATEGORIES, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      KNOWS_DATA_QUANTITY,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      DATA_QUANTITY,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES), DependencyEntry(KNOWS_DATA_QUANTITY, YES)),
//      DataQuantityAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      DELIVERY_METHOD,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES)),
//      DeliveryMethodAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      DONE_DPIA,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      DEFINED_DATA_RETENTION_PERIOD,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES)),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      PURPOSE_PURSUIT,
//      required = true,
//      Seq(DependencyEntry(USES_PERSONAL_DATA, YES)),
//      PurposePursuitAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      CHECKED_EXISTENCE_MERE_CORRECTNESS_INTEROP_CATALOGUE,
//      required = true,
//      Seq(
//        DependencyEntry(USES_PERSONAL_DATA, YES),
//        DependencyEntry(PURPOSE_PURSUIT, PurposePursuitAnswer.MERE_CORRECTNESS.toString)
//      ),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      CHECKED_ALL_DATA_NEEDED,
//      required = true,
//      Seq(
//        DependencyEntry(USES_PERSONAL_DATA, YES),
//        DependencyEntry(PURPOSE_PURSUIT, PurposePursuitAnswer.NEW_PERSONAL_DATA.toString)
//      ),
//      YesNoAnswer.valuesSet.some
//    ),
//    ValidationEntry(
//      CHECKED_EXISTENCE_MINIMAL_DATA_INTEROP_CATALOGUE,
//      required = true,
//      Seq(
//        DependencyEntry(USES_PERSONAL_DATA, YES),
//        DependencyEntry(PURPOSE_PURSUIT, PurposePursuitAnswer.NEW_PERSONAL_DATA.toString),
//        DependencyEntry(CHECKED_ALL_DATA_NEEDED, NO)
//      ),
//      YesNoAnswer.valuesSet.some
//    )
//  )
//}
//
//object ValidationRulesV2 {
//
//  // Answers
//  object YesNoAnswer extends Enumeration {
//    type YesNo = Value
//    val YES, NO = Value
//  }
//
//  val YES: String = YesNoAnswer.YES.toString
//  val NO: String  = YesNoAnswer.NO.toString
//
//  object FormLegalBasisAnswer extends Enumeration {
//    type FormLegalBasisAnswers = Value
//    val CONSENT, CONTRACT, LEGAL_OBLIGATION, SAFEGUARD, PUBLIC_INTEREST, LEGITIMATE_INTEREST = Value
//  }
//
//  object FormDataQuantityAnswer extends Enumeration {
//    type FormDataQuantityAnswers = Value
//    val QUANTITY_0_TO_100, QUANTITY_101_TO_500, QUANTITY_500_TO_1000, QUANTITY_1001_TO_5000, QUANTITY_5001_OVER = Value
//  }
//
//  object FormDeliveryMethodAnswer extends Enumeration {
//    type FormDeliveryMethodAnswers = Value
//    val CLEARTEXT, AGGREGATE, ANONYMOUS, PSEUDOANONYMOUS = Value
//  }
//
//  object FormPurposePursuitAnswer extends Enumeration {
//    type FormPurposePursuitAnswers = Value
//    val MERE_CORRECTNESS, NEW_PERSONAL_DATA = Value
//  }
//  // End Answers
//
//  // Fields names
//  val PURPOSE: String = "purpose"
//  // End Fields names
//
//  val validationRules: List[ValidationEntry] = List(
////    ValidationEntry(PURPOSE, required = true, Seq.empty),
//  )
//}
