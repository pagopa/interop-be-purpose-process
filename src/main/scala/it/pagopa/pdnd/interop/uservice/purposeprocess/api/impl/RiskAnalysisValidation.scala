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

  val validationGraph: List[ValidationEntry] = List(
    ValidationEntry("purpose", required = true, Seq.empty),
    ValidationEntry("usesPersonalData", required = true, Seq.empty),
    ValidationEntry("usesThirdPartyPersonalData", required = true, Seq(DependencyEntry("usesPersonalData", NO))),
    ValidationEntry(
      "usesConfidentialData",
      required = true,
      Seq(DependencyEntry("usesPersonalData", NO), DependencyEntry("usesThirdPartyPersonalData", YES))
    ),
    ValidationEntry("securedDataAccess", required = true, Seq(DependencyEntry("usesPersonalData", NO))),
    ValidationEntry("legalBasis", required = true, Seq(DependencyEntry("usesPersonalData", YES))),
    ValidationEntry(
      "legalObligationReference",
      required = true,
      Seq(DependencyEntry("legalBasis", FormLegalBasisAnswers.LEGAL_OBLIGATION.toString))
    ),
    ValidationEntry(
      "publicInterestReference",
      required = true,
      Seq(DependencyEntry("legalBasis", FormLegalBasisAnswers.PUBLIC_INTEREST.toString))
    ),
    ValidationEntry("knowsAccessedDataCategories", required = true, Seq(DependencyEntry("usesPersonalData", YES))),
    ValidationEntry(
      "accessDataArt9Gdpr",
      required = true,
      Seq(DependencyEntry("usesPersonalData", YES), DependencyEntry("knowsAccessedDataCategories", YES))
    ),
    ValidationEntry(
      "accessUnderageData",
      required = true,
      Seq(DependencyEntry("usesPersonalData", YES), DependencyEntry("knowsAccessedDataCategories", YES))
    ),
    ValidationEntry("knowsDataQuantity", required = true, Seq(DependencyEntry("usesPersonalData", YES))),
    ValidationEntry(
      "dataQuantity",
      required = true,
      Seq(DependencyEntry("usesPersonalData", YES), DependencyEntry("knowsDataQuantity", YES))
    ),
    ValidationEntry("deliveryMethod", required = true, Seq(DependencyEntry("usesPersonalData", YES))),
    ValidationEntry("doneDpia", required = true, Seq(DependencyEntry("usesPersonalData", YES))),
    ValidationEntry("definedDataRetentionPeriod", required = true, Seq(DependencyEntry("usesPersonalData", YES))),
    ValidationEntry("purposePursuit", required = true, Seq(DependencyEntry("usesPersonalData", YES))),
    ValidationEntry(
      "checkedExistenceMereCorrectnessInteropCatalogue",
      required = true,
      Seq(
        DependencyEntry("usesPersonalData", YES),
        DependencyEntry("purposePursuit", FormPurposePursuitAnswers.MERE_CORRECTNESS.toString)
      )
    ),
    ValidationEntry(
      "checkedAllDataNeeded",
      required = true,
      Seq(
        DependencyEntry("usesPersonalData", YES),
        DependencyEntry("purposePursuit", FormPurposePursuitAnswers.NEW_PERSONAL_DATA.toString)
      )
    ),
    ValidationEntry(
      "checkedExistenceMinimalDataInteropCatalogue",
      required = true,
      Seq(
        DependencyEntry("usesPersonalData", YES),
        DependencyEntry("purposePursuit", FormPurposePursuitAnswers.NEW_PERSONAL_DATA.toString),
        DependencyEntry("checkedAllDataNeeded", NO)
      )
    )
  )
}
