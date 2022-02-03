package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import cats.data._
import cats.implicits._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysisForm,
  RiskAnalysisMultiAnswer => MultiAnswer,
  RiskAnalysisSingleAnswer => SingleAnswer
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.error._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import spray.json._

object RiskAnalysisValidation {
  import ValidationRules._

  type ValidationResult[A] = ValidatedNec[RiskAnalysisValidationError, A]

  def validate(form: RiskAnalysisForm): ValidationResult[DepRiskAnalysisForm] = {
    val answersJson: JsObject = form.answers.toJson.asJsObject

    val validations = validateForm(answersJson, validationGraph)

    val singleAnswers: ValidationResult[Seq[SingleAnswer]] = validations.collect { case Left(s) => s }.sequence
    val multiAnswers: ValidationResult[Seq[MultiAnswer]]   = validations.collect { case Right(m) => m }.sequence
    val expectedFields: ValidationResult[List[Unit]]       = validateExpectedFields(answersJson, validationGraph)

    (singleAnswers, multiAnswers, expectedFields).mapN((l1, l2, _) =>
      DepRiskAnalysisForm(version = form.version, singleAnswers = l1, multiAnswers = l2)
    )
  }

  /** Verify if field is present when its dependencies are met */
  def validateExpectedFields(
    answersJson: JsObject,
    validationTree: List[ValidationEntry]
  ): ValidationResult[List[Unit]] = {
    validationTree
      .filter(_.required)
      .map { entry =>
        val dependenciesSatisfied: Boolean =
          entry.dependencies.forall { dep =>
            answersJson.getFields(dep.fieldName).exists { f =>
              f match {
                case str: JsString => str.value == dep.fieldValue
                case arr: JsArray  => jsArrayContains(arr, dep.fieldValue)
                case _             => false
              }
            }
          }

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

  def validateForm(
    answersJson: JsObject,
    validationTree: List[ValidationEntry]
  ): Seq[Either[ValidationResult[SingleAnswer], ValidationResult[MultiAnswer]]] = {
    answersJson.fields.map { case (key, value) =>
      validateField(answersJson, validationTree)(key).fold(
        err => Left[ValidationResult[SingleAnswer], Nothing](err.invalid),
        _ =>
          value match {
            case str: JsString =>
              Left(SingleAnswer(key, Some(str.value)).validNec[RiskAnalysisValidationError])
            case arr: JsArray =>
              val values: Seq[ValidationResult[String]] = {
                arr.elements.map {
                  case str: JsString => str.value.validNec
                  case _             => UnexpectedFieldFormat(key).invalidNec
                }
              }
              Right(values.sequence.map(MultiAnswer(key, _)))
            case _ => Left(UnexpectedFieldFormat(key).invalidNec)
          }
      )
    }.toSeq
  }

  def validateField(answersJson: JsObject, validationTree: List[ValidationEntry])(
    key: String
  ): ValidationResult[Seq[Unit]] =
    validationTree.find(_.fieldName == key) match {
      case Some(rule) =>
        rule.dependencies.map(validateDependency(answersJson: JsObject)).sequence
      case None =>
        UnexpectedField(key).invalidNec
    }

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

  val validationGraph: List[ValidationEntry] = List(
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
