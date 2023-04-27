package it.pagopa.interop.purposeprocess

import cats.data.NonEmptyChain
import cats.implicits.catsSyntaxOptionId
import cats.kernel.Eq
import it.pagopa.interop.purposemanagement.client.model.{
  RiskAnalysisFormSeed,
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
    "succeed on correct form 1.0" in {
      val riskAnalysis = SpecData.validRiskAnalysis1_0

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "MyPurpose".some),
          SingleAnswerSeed("usesPersonalData", Some("YES")),
          SingleAnswerSeed("legalObligationReference", "somethingLegal".some),
          SingleAnswerSeed("publicInterestReference", "somethingPublic".some),
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

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, false)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 1.0 only schema" in {
      val riskAnalysis = SpecData.validRiskAnalysis1_0

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "MyPurpose".some),
          SingleAnswerSeed("usesPersonalData", Some("YES")),
          SingleAnswerSeed("legalObligationReference", "somethingLegal".some),
          SingleAnswerSeed("publicInterestReference", "somethingPublic".some),
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

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, true)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 2.0" in {
      val riskAnalysis = SpecData.validRiskAnalysis2_0

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "INSTITUTIONAL".some),
          SingleAnswerSeed("institutionalPurpose", "MyPurpose".some),
          SingleAnswerSeed("otherPersonalDataTypes", "MyDataTypes".some),
          SingleAnswerSeed("legalObligationReference", "somethingLegal".some),
          SingleAnswerSeed("legalBasisPublicInterest", "RULE_OF_LAW".some),
          SingleAnswerSeed("ruleOfLawText", "TheLaw".some),
          SingleAnswerSeed("knowsDataQuantity", "NO".some),
          SingleAnswerSeed("deliveryMethod", "ANONYMOUS".some),
          SingleAnswerSeed("policyProvided", "NO".some),
          SingleAnswerSeed("confirmPricipleIntegrityAndDiscretion", "true".some),
          SingleAnswerSeed("reasonPolicyNotProvided", "Because".some),
          SingleAnswerSeed("doneDpia", "NO".some),
          SingleAnswerSeed("dataRetentionPeriod", "true".some),
          SingleAnswerSeed("purposePursuit", "MERE_CORRECTNESS".some),
          SingleAnswerSeed("checkedExistenceMereCorrectnessInteropCatalogue", "true".some),
          SingleAnswerSeed("usesThirdPartyData", "NO".some),
          SingleAnswerSeed("declarationConfirmGDPR", "true".some)
        ),
        multiAnswers = Seq(
          MultiAnswerSeed("personalDataTypes", Seq("OTHER")),
          MultiAnswerSeed("legalBasis", Seq("LEGAL_OBLIGATION", "PUBLIC_INTEREST"))
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, false)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 2.0 only schema" in {
      val riskAnalysis = SpecData.validRiskAnalysis2_0

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "INSTITUTIONAL".some),
          SingleAnswerSeed("institutionalPurpose", "MyPurpose".some),
          SingleAnswerSeed("otherPersonalDataTypes", "MyDataTypes".some),
          SingleAnswerSeed("legalObligationReference", "somethingLegal".some),
          SingleAnswerSeed("legalBasisPublicInterest", "RULE_OF_LAW".some),
          SingleAnswerSeed("ruleOfLawText", "TheLaw".some),
          SingleAnswerSeed("knowsDataQuantity", "NO".some),
          SingleAnswerSeed("deliveryMethod", "ANONYMOUS".some),
          SingleAnswerSeed("policyProvided", "NO".some),
          SingleAnswerSeed("confirmPricipleIntegrityAndDiscretion", "true".some),
          SingleAnswerSeed("reasonPolicyNotProvided", "Because".some),
          SingleAnswerSeed("doneDpia", "NO".some),
          SingleAnswerSeed("dataRetentionPeriod", "true".some),
          SingleAnswerSeed("purposePursuit", "MERE_CORRECTNESS".some),
          SingleAnswerSeed("checkedExistenceMereCorrectnessInteropCatalogue", "true".some),
          SingleAnswerSeed("usesThirdPartyData", "NO".some),
          SingleAnswerSeed("declarationConfirmGDPR", "true".some)
        ),
        multiAnswers = Seq(
          MultiAnswerSeed("personalDataTypes", Seq("OTHER")),
          MultiAnswerSeed("legalBasis", Seq("LEGAL_OBLIGATION", "PUBLIC_INTEREST"))
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, true)

      verifyValidationFormResult(result, expected)

    }

    "fail if a provided answer depends on a missing field" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("YES"),
          "usesThirdPartyPersonalData" -> Nil,
          "usesConfidentialData"       -> List("YES")
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, false)

      verifyValidationFailure(
        result,
        _.contains(DependencyNotFound("usesThirdPartyPersonalData", "usesConfidentialData")) shouldBe true
      )
    }

    "succeed only schema (fail if a provided answer depends on a missing field)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("YES"),
          "usesThirdPartyPersonalData" -> Nil,
          "usesConfidentialData"       -> List("YES")
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, true)

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "purpose".some),
          SingleAnswerSeed("usesPersonalData", "YES".some),
          SingleAnswerSeed("usesConfidentialData", "YES".some)
        ),
        multiAnswers = Seq.empty
      )

      verifyValidationFormResult(result, expected)
    }

    "fail if a provided answer depends on an existing field with an unexpected value" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("NO"),
          "usesThirdPartyPersonalData" -> List("NO"),
          "usesConfidentialData"       -> List("YES")
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, false)

      verifyValidationFailure(
        result,
        _.contains(
          UnexpectedFieldValueByDependency("usesThirdPartyPersonalData", "usesConfidentialData", "YES")
        ) shouldBe true
      )
    }

    "succeed only schema (complete validation should fail because provided answer depends on an existing field with an unexpected value)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("NO"),
          "usesThirdPartyPersonalData" -> List("NO"),
          "usesConfidentialData"       -> List("YES")
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, true)

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "purpose".some),
          SingleAnswerSeed("usesPersonalData", "NO".some),
          SingleAnswerSeed("usesThirdPartyPersonalData", "NO".some),
          SingleAnswerSeed("usesConfidentialData", "YES".some)
        ),
        multiAnswers = Seq.empty
      )

      verifyValidationFormResult(result, expected)

    }

    "fail on missing expected answer (answer tree is not complete)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("NO"),
          "usesThirdPartyPersonalData" -> List("YES"),
          "usesConfidentialData"       -> Nil,
          "securedDataAccess"          -> Nil
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, false)

      verifyValidationFailure(
        result,
        err =>
          (err.contains(MissingExpectedField("usesConfidentialData")) &&
            err.contains(MissingExpectedField("securedDataAccess"))) shouldBe true
      )

    }

    "succeed only schema (complete validation should fail because missing expected answer, as tree is not complete)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("NO"),
          "usesThirdPartyPersonalData" -> List("YES"),
          "usesConfidentialData"       -> Nil,
          "securedDataAccess"          -> Nil
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, true)

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "purpose".some),
          SingleAnswerSeed("usesPersonalData", "NO".some),
          SingleAnswerSeed("usesThirdPartyPersonalData", "YES".some)
        ),
        multiAnswers = Seq.empty
      )

      verifyValidationFormResult(result, expected)

    }

    "fail on unexpected field name in only schema validation" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose1"                   -> List("purpose"),
          "usesPersonalData"           -> List("NO"),
          "usesThirdPartyPersonalData" -> List("YES"),
          "usesConfidentialData"       -> Nil,
          "securedDataAccess"          -> Nil
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, true)

      verifyValidationFailure(result, _.contains(UnexpectedField("purpose1")) shouldBe true)

    }

    "fail on unexpected field value in only schema validation" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("pippo"),
          "usesThirdPartyPersonalData" -> List("YES"),
          "usesConfidentialData"       -> Nil,
          "securedDataAccess"          -> Nil
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] = RiskAnalysisValidation.validate(riskAnalysis, true)

      verifyValidationFailure(
        result,
        _.contains(UnexpectedFieldValue("usesPersonalData", Option(Set("YES", "NO")))) shouldBe true
      )

    }
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
