package it.pagopa.pdnd.interop.uservice.purposeprocess

import cats.data.NonEmptyChain
import cats.kernel.Eq
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysisForm,
  RiskAnalysisMultiAnswer => MultiAnswer,
  RiskAnalysisSingleAnswer => SingleAnswer
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.RiskAnalysisValidation.ValidationResult
import it.pagopa.pdnd.interop.uservice.purposeprocess.error._
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

class RiskAnalysisValidationSpec extends AnyWordSpecLike {
  implicit val eqError: Eq[RiskAnalysisValidationError] = Eq.fromUniversalEquals

  "Risk Analysis Validation" should {
    "succeed on correct form" in {

      val riskAnalysis = SpecData.validRiskAnalysis

      val expected = DepRiskAnalysisForm(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswer("purpose", Some(riskAnalysis.answers.purpose)),
          SingleAnswer("usesPersonalData", Some(riskAnalysis.answers.usesPersonalData.toString)),
          SingleAnswer("legalObligationReference", riskAnalysis.answers.legalObligationReference),
          SingleAnswer("publicInterestReference", riskAnalysis.answers.publicInterestReference),
          SingleAnswer("knowsAccessedDataCategories", riskAnalysis.answers.knowsAccessedDataCategories.map(_.toString)),
          SingleAnswer("accessDataArt9Gdpr", riskAnalysis.answers.accessDataArt9Gdpr.map(_.toString)),
          SingleAnswer("accessUnderageData", riskAnalysis.answers.accessUnderageData.map(_.toString)),
          SingleAnswer("knowsDataQuantity", riskAnalysis.answers.knowsDataQuantity.map(_.toString)),
          SingleAnswer("deliveryMethod", riskAnalysis.answers.deliveryMethod.map(_.toString)),
          SingleAnswer("doneDpia", riskAnalysis.answers.doneDpia.map(_.toString)),
          SingleAnswer("definedDataRetentionPeriod", riskAnalysis.answers.definedDataRetentionPeriod.map(_.toString)),
          SingleAnswer("purposePursuit", riskAnalysis.answers.purposePursuit.map(_.toString))
        ),
        multiAnswers = Seq(
          MultiAnswer("legalBasis", riskAnalysis.answers.legalBasis.toList.flatten.map(_.toString)),
          MultiAnswer(
            "checkedExistenceMereCorrectnessInteropCatalogue",
            riskAnalysis.answers.checkedExistenceMereCorrectnessInteropCatalogue.toList.flatten.map(_.toString)
          )
        )
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      verifyValidationFormResult(result, expected)

    }

    "fail if a provided answer depends on a missing field" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = RiskAnalysisFormAnswers(
          purpose = "purpose",
          usesPersonalData = RiskAnalysisFormYesNoAnswer.YES,
          usesThirdPartyPersonalData = None,
          usesConfidentialData = Some(RiskAnalysisFormYesNoAnswer.YES)
        )
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      verifyValidationFailure(result, _.contains(DependencyNotFound("usesThirdPartyPersonalData")) shouldBe true)
    }

    "fail if a provided answer depends on an existing field with an unexpected value" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = RiskAnalysisFormAnswers(
          purpose = "purpose",
          usesPersonalData = RiskAnalysisFormYesNoAnswer.NO,
          usesThirdPartyPersonalData = Some(RiskAnalysisFormYesNoAnswer.NO),
          usesConfidentialData = Some(RiskAnalysisFormYesNoAnswer.YES)
        )
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      verifyValidationFailure(result, _.contains(UnexpectedFieldValue("usesThirdPartyPersonalData")) shouldBe true)
    }

    "fail on missing expected answer (answer tree is not complete)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = RiskAnalysisFormAnswers(
          purpose = "purpose",
          usesPersonalData = RiskAnalysisFormYesNoAnswer.NO,
          usesThirdPartyPersonalData = Some(RiskAnalysisFormYesNoAnswer.YES),
          usesConfidentialData = None,
          securedDataAccess = None
        )
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      verifyValidationFailure(
        result,
        err =>
          (err.contains(MissingExpectedField("usesConfidentialData")) &&
            err.contains(MissingExpectedField("securedDataAccess"))) shouldBe true
      )

    }

    // Fields are all required for now
//    "succeed if not required field is missing" in {}

  }

  def verifyValidationFormResult(
    result: ValidationResult[DepRiskAnalysisForm],
    expected: DepRiskAnalysisForm
  ): Assertion = {
    result.fold(
      err => fail(s"Unexpected validation failure: ${err.toString}"),
      r => {
        r.version shouldBe expected.version
        r.singleAnswers should contain theSameElementsAs expected.singleAnswers
        r.multiAnswers should contain theSameElementsAs expected.multiAnswers
      }
    )
  }

  def verifyValidationFailure(
    result: ValidationResult[DepRiskAnalysisForm],
    errorAssertion: NonEmptyChain[RiskAnalysisValidationError] => Assertion
  ): Assertion =
    result.fold(errorAssertion, result => fail(s"Unexpected validation success $result"))

}
