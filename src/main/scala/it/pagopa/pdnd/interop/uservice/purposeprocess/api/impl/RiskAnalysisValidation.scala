package it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl

import cats.data.Validated.Valid
import cats.data._
import cats.implicits._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysisForm,
  RiskAnalysisMultiAnswer => MultiAnswer,
  RiskAnalysisSingleAnswer => SingleAnswer
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import spray.json.{JsArray, JsObject, JsString, enrichAny}

object RiskAnalysisValidation {
  type ValidationResult[A] = ValidatedNec[RiskAnalysisValidation, A]

  final case class DependencyEntry(fieldName: String, fieldValue: String)
  final case class ValidationEntry(fieldName: String, required: Boolean, dependencies: Seq[DependencyEntry])

  val validationTree: List[ValidationEntry] = List(
    ValidationEntry("purpose", required = true, Seq.empty),
    ValidationEntry("usesPersonalData", required = true, Seq.empty),
    ValidationEntry(
      "usesThirdPartyPersonalData",
      required = true,
      Seq(DependencyEntry("usesPersonalData", "usesPersonalDataNo"))
    ),
    ValidationEntry(
      "usesConfidentialData",
      required = true,
      Seq(DependencyEntry("usesThirdPartyPersonalData", "usesThirdPartyPersonalDataYes"))
    )
  )

  def validate(form: RiskAnalysisForm): ValidationResult[DepRiskAnalysisForm] = {
    val answersJson: JsObject = form.answers.toJson.asJsObject

    val validations = validateForm(answersJson, validationTree)

    val singleAnswers: ValidationResult[Seq[SingleAnswer]] = validations.collect { case Left(s) => s }.sequence
    val multiAnswers: ValidationResult[Seq[MultiAnswer]]   = validations.collect { case Right(m) => m }.sequence
    val expectedFields: ValidationResult[List[Unit]]       = validateExpectedFields(answersJson, validationTree)

    (singleAnswers, multiAnswers, expectedFields).mapN((l1, l2, _) =>
      DepRiskAnalysisForm(version = form.version, singleAnswers = l1, multiAnswers = l2)
    )
  }

  /** Verify that if field is present when its dependencies are met
    */
  def validateExpectedFields(
    answersJson: JsObject,
    validationTree: List[ValidationEntry]
  ): ValidationResult[List[Unit]] = {
    validationTree.map { entry =>
      val allFieldDependenciesSatisfied: Boolean = entry.dependencies.forall { dep =>
        answersJson.getFields(dep.fieldName).exists { f =>
          f match {
            case str: JsString => str.value == dep.fieldValue
            case arr: JsArray  => arrayContainsValue(arr, dep.fieldValue)
            case _             => false
          }
//        answersJson.getFields(dep.fieldName) match {
//          case Nil => false
//          case f :: Nil =>
//            f match {
//              case str: JsString => str.value == dep.fieldValue
//              case arr: JsArray  => arrayContainsValue(arr, dep.fieldValue)
//              case _             => false
//            }
//          case _ :: _ => false
        }
      }

      if (
        !allFieldDependenciesSatisfied ||
        (allFieldDependenciesSatisfied && answersJson.getFields(entry.fieldName).nonEmpty)
      )
        ().validNec
      else
        MissingExpectedField(entry.fieldName).invalidNec
    }.sequence
  }

  // TODO Check `required` field

  def validateForm(
    answersJson: JsObject,
    validationTree: List[ValidationEntry]
  ): Seq[Either[ValidationResult[SingleAnswer], ValidationResult[MultiAnswer]]] = {
    answersJson.fields.map { case (key, value) =>
      validateField(key)(answersJson, validationTree) match {
        case Valid(_) =>
          value match {
            case str: JsString =>
              Left(SingleAnswer(key, Some(str.value)).validNec[RiskAnalysisValidation])
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
        case err: ValidatedNec[_, _] =>
          Left[ValidationResult[SingleAnswer], Nothing](err.as(SingleAnswer("", None))) // TODO Fix this
      }
    }.toSeq
  }

  def validateField(
    key: String
  )(answersJson: JsObject, validationTree: List[ValidationEntry]): ValidationResult[Seq[Unit]] =
    validationTree.find(_.fieldName == key) match {
      case Some(rule) =>
        rule.dependencies.map(verifyDependency(answersJson: JsObject)).sequence
      case None =>
        UnexpectedField(key).invalidNec
    }

  def verifyDependency(answersJson: JsObject)(dependency: DependencyEntry): ValidationResult[Unit] = {
    val fields = answersJson.getFields(dependency.fieldName)
    fields match {
      case Nil => DependencyNotFound(dependency.fieldName).invalidNec
      case f :: Nil =>
        f match {
          case str: JsString if str.value == dependency.fieldValue =>
            ().validNec
          case arr: JsArray if arrayContainsValue(arr, dependency.fieldValue) =>
            ().validNec
          case _ =>
            UnexpectedFieldValue(dependency.fieldName).invalidNec
        }
      case _ :: _ => TooManyOccurrences(dependency.fieldName).invalidNec
    }
  }

  def arrayContainsValue(arr: JsArray, value: String): Boolean =
    arr.elements.collectFirst { case v: JsString if v.value == value => () }.nonEmpty

  final case class UnexpectedField(fieldName: String)       extends RiskAnalysisValidation
  final case class DependencyNotFound(fieldName: String)    extends RiskAnalysisValidation
  final case class TooManyOccurrences(fieldName: String)    extends RiskAnalysisValidation
  final case class MissingExpectedField(fieldName: String)  extends RiskAnalysisValidation
  final case class UnexpectedFieldValue(fieldName: String)  extends RiskAnalysisValidation
  final case class UnexpectedFieldFormat(fieldName: String) extends RiskAnalysisValidation

//  def validateSingleAnswer(
//    tree: List[RiskAnalysisValidation.ValidationEntry],
//    key: String,
//    value: Option[String]
//  ): ValidationResult[List[SingleAnswer]] =
//    tree.find(_.fieldName == key) match {
//      case Some(rule) =>
//        rule.dependencies.map()
//      case None => UnexpectedField(key).invalidNec
//    }

//  def validate(form: RiskAnalysisForm): ValidationResult[DepRiskAnalysisForm] = {
//    val singleAnswers: ValidationResult[List[SingleAnswer]] =
//      List(
//        validatePurpose(form.answers.purpose),
//        validateUsesThirdPartyPersonalData(form.answers.usesPersonalData, form.answers.usesThirdPartyPersonalData)
//      ).sequence.map(_.flatten)
//
//    val multiAnswers: ValidationResult[List[MultiAnswer]] = List.empty.validNec
//
//    (singleAnswers, multiAnswers).mapN((l1, l2) =>
//      DepRiskAnalysisForm(version = form.version, singleAnswers = l1, multiAnswers = l2)
//    )
//  }

//  def validatePurpose(purpose: String): ValidationResult[Option[SingleAnswer]] =
//    Some(SingleAnswer(QuestionsNames.Purpose, Some(purpose))).validNec
//
//  def validateUsesThirdPartyPersonalData(
//    personalData: FormUsesPersonalDataAnswers,
//    thirdParty: Option[FormUsesThirdPartyPersonalDataAnswers]
//  ): ValidationResult[Option[SingleAnswer]] =
//    (personalData, thirdParty) match {
//      case (FormUsesPersonalDataAnswers.uses_personal_data_no, Some(answer)) =>
//        Some(SingleAnswer(QuestionsNames.UsesThirdPartyPersonalData, Some(answer.toString))).validNec
//      case (FormUsesPersonalDataAnswers.uses_personal_data_no, None) =>
//        UsesThirdPartyPersonalDataMissing.invalidNec
//      case _ => None.validNec
//    }
}

sealed trait RiskAnalysisValidation
//{
//  def errorMessage: String
//}

case object UsesThirdPartyPersonalDataMissing extends RiskAnalysisValidation {
  def errorMessage: String = s"${QuestionsNames.UsesThirdPartyPersonalData} is required"
}

object QuestionsNames {
  val Purpose: String                    = "purpose"
  val UsesPersonalData: String           = "uses_personal_data"
  val UsesThirdPartyPersonalData: String = "uses_third_party_personal_data"
}
