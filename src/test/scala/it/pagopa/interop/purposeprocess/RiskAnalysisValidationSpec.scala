package it.pagopa.interop.purposeprocess

import cats.data.NonEmptyChain
import cats.kernel.Eq
import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisFormSeed => RiskAnalysisFormSeed,
  RiskAnalysisMultiAnswerSeed => MultiAnswerSeed,
  RiskAnalysisSingleAnswerSeed => SingleAnswerSeed
}
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.interop.purposeprocess.api.impl.RiskAnalysisValidation.ValidationResult
import it.pagopa.interop.purposeprocess.error._
import it.pagopa.interop.purposeprocess.model._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

class RiskAnalysisValidationSpec extends AnyWordSpecLike {
  implicit val eqError: Eq[RiskAnalysisValidationError] = Eq.fromUniversalEquals

  "Risk Analysis Validation" should {
    "succeed on correct form" in {
      val riskAnalysis = SpecData.validRiskAnalysis

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", Some(riskAnalysis.answers.purpose)),
          SingleAnswerSeed("usesPersonalData", Some("YES")),
          SingleAnswerSeed("legalObligationReference", riskAnalysis.answers.legalObligationReference),
          SingleAnswerSeed("publicInterestReference", riskAnalysis.answers.publicInterestReference),
          SingleAnswerSeed("knowsAccessedDataCategories", Some("YES")),
          SingleAnswerSeed("accessDataArt9Gdpr", Some("NO")),
          SingleAnswerSeed("accessUnderageData", Some("NO")),
          SingleAnswerSeed("knowsDataQuantity", Some("NO")),
          SingleAnswerSeed("deliveryMethod", Some("ANONYMOUS")),
          SingleAnswerSeed("doneDpia", Some("NO")),
          SingleAnswerSeed("definedDataRetentionPeriod", Some("NO")),
          SingleAnswerSeed("purposePursuit", Some("MERE_CORRECTNESS"))
        ),
        multiAnswers = Seq(
          MultiAnswerSeed("legalBasis", Seq("LEGAL_OBLIGATION", "PUBLIC_INTEREST")),
          MultiAnswerSeed("checkedExistenceMereCorrectnessInteropCatalogue", Seq("YES"))
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis)

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

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis)

      verifyValidationFailure(
        result,
        _.contains(DependencyNotFound("usesThirdPartyPersonalData", "usesConfidentialData")) shouldBe true
      )
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

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis)

      verifyValidationFailure(
        result,
        _.contains(UnexpectedFieldValue("usesThirdPartyPersonalData", "usesConfidentialData", "YES")) shouldBe true
      )
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

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis)

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
    result: ValidationResult[RiskAnalysisFormSeed],
    expected: RiskAnalysisFormSeed
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
    result: ValidationResult[RiskAnalysisFormSeed],
    errorAssertion: NonEmptyChain[RiskAnalysisValidationError] => Assertion
  ): Assertion =
    result.fold(errorAssertion, result => fail(s"Unexpected validation success $result"))

}
