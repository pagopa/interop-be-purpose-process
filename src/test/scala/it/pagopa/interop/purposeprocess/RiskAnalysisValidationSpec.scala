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
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import it.pagopa.interop.purposeprocess.model.RiskAnalysisForm
import it.pagopa.interop.tenantmanagement.model.tenant.PersistentTenantKind

class RiskAnalysisValidationSpec extends AnyWordSpecLike {
  implicit val eqError: Eq[RiskAnalysisValidationError] = Eq.fromUniversalEquals

  "Risk Analysis Validation" should {
    "succeed on correct form 2.0 on tenant kind PA" in {

      val riskAnalysis: RiskAnalysisForm = SpecData.validRiskAnalysis2_0_Pa

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("legalObligationReference", Some("somethingLegal")),
          SingleAnswerSeed("checkedExistenceMereCorrectnessInteropCatalogue", Some("true")),
          SingleAnswerSeed("deliveryMethod", Some("ANONYMOUS")),
          SingleAnswerSeed("legalBasisPublicInterest", Some("RULE_OF_LAW")),
          SingleAnswerSeed("confirmPricipleIntegrityAndDiscretion", Some("true")),
          SingleAnswerSeed("usesThirdPartyData", Some("NO")),
          SingleAnswerSeed("purpose", Some("INSTITUTIONAL")),
          SingleAnswerSeed("ruleOfLawText", Some("TheLaw")),
          SingleAnswerSeed("dataRetentionPeriod", Some("true")),
          SingleAnswerSeed("otherPersonalDataTypes", Some("MyDataTypes")),
          SingleAnswerSeed("knowsDataQuantity", Some("NO")),
          SingleAnswerSeed("institutionalPurpose", Some("MyPurpose")),
          SingleAnswerSeed("policyProvided", Some("NO")),
          SingleAnswerSeed("reasonPolicyNotProvided", Some("Because")),
          SingleAnswerSeed("doneDpia", Some("NO")),
          SingleAnswerSeed("declarationConfirmGDPR", Some("true")),
          SingleAnswerSeed("purposePursuit", Some("MERE_CORRECTNESS"))
        ),
        multiAnswers = Seq(
          MultiAnswerSeed("personalDataTypes", Seq("OTHER")),
          MultiAnswerSeed("legalBasis", Seq("LEGAL_OBLIGATION", "PUBLIC_INTEREST"))
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PA)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 2.0 only schema on tenant kind PA" in {
      val riskAnalysis = SpecData.validOnlySchemaRiskAnalysis2_0

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(SingleAnswerSeed("purpose", Some("INSTITUTIONAL"))),
        multiAnswers = Seq.empty
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PA)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 1.0 on tenant kind PRIVATE" in {
      val riskAnalysis = SpecData.validRiskAnalysis1_0_Private

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("legalObligationReference", Some("somethingLegal")),
          SingleAnswerSeed("usesPersonalData", Some("YES")),
          SingleAnswerSeed("dataDownload", Some("YES")),
          SingleAnswerSeed("checkedExistenceMereCorrectnessInteropCatalogue", Some("true")),
          SingleAnswerSeed("deliveryMethod", Some("ANONYMOUS")),
          SingleAnswerSeed("legalBasisPublicInterest", Some("RULE_OF_LAW")),
          SingleAnswerSeed("confirmPricipleIntegrityAndDiscretion", Some("true")),
          SingleAnswerSeed("purpose", Some("INSTITUTIONAL")),
          SingleAnswerSeed("ruleOfLawText", Some("TheLaw")),
          SingleAnswerSeed("dataRetentionPeriod", Some("true")),
          SingleAnswerSeed("otherPersonalDataTypes", Some("MyDataTypes")),
          SingleAnswerSeed("knowsDataQuantity", Some("NO")),
          SingleAnswerSeed("institutionalPurpose", Some("MyPurpose")),
          SingleAnswerSeed("policyProvided", Some("NO")),
          SingleAnswerSeed("reasonPolicyNotProvided", Some("Because")),
          SingleAnswerSeed("doneDpia", Some("NO")),
          SingleAnswerSeed("declarationConfirmGDPR", Some("true")),
          SingleAnswerSeed("purposePursuit", Some("MERE_CORRECTNESS"))
        ),
        multiAnswers = Seq(
          MultiAnswerSeed("personalDataTypes", Seq("OTHER")),
          MultiAnswerSeed("legalBasis", Seq("LEGAL_OBLIGATION", "PUBLIC_INTEREST"))
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, false)(PersistentTenantKind.PRIVATE)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 1.0 only schema on tenant kind PRIVATE" in {
      val riskAnalysis = SpecData.validOnlySchemaRiskAnalysis1_0

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(SingleAnswerSeed("purpose", Some("INSTITUTIONAL"))),
        multiAnswers = Seq.empty
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PRIVATE)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 1.0 on tenant kind GSP" in {
      val riskAnalysis = SpecData.validRiskAnalysis1_0_Private

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("legalObligationReference", Some("somethingLegal")),
          SingleAnswerSeed("usesPersonalData", Some("YES")),
          SingleAnswerSeed("dataDownload", Some("YES")),
          SingleAnswerSeed("checkedExistenceMereCorrectnessInteropCatalogue", Some("true")),
          SingleAnswerSeed("deliveryMethod", Some("ANONYMOUS")),
          SingleAnswerSeed("legalBasisPublicInterest", Some("RULE_OF_LAW")),
          SingleAnswerSeed("confirmPricipleIntegrityAndDiscretion", Some("true")),
          SingleAnswerSeed("purpose", Some("INSTITUTIONAL")),
          SingleAnswerSeed("ruleOfLawText", Some("TheLaw")),
          SingleAnswerSeed("dataRetentionPeriod", Some("true")),
          SingleAnswerSeed("otherPersonalDataTypes", Some("MyDataTypes")),
          SingleAnswerSeed("knowsDataQuantity", Some("NO")),
          SingleAnswerSeed("institutionalPurpose", Some("MyPurpose")),
          SingleAnswerSeed("policyProvided", Some("NO")),
          SingleAnswerSeed("reasonPolicyNotProvided", Some("Because")),
          SingleAnswerSeed("doneDpia", Some("NO")),
          SingleAnswerSeed("declarationConfirmGDPR", Some("true")),
          SingleAnswerSeed("purposePursuit", Some("MERE_CORRECTNESS"))
        ),
        multiAnswers = Seq(
          MultiAnswerSeed("personalDataTypes", Seq("OTHER")),
          MultiAnswerSeed("legalBasis", Seq("LEGAL_OBLIGATION", "PUBLIC_INTEREST"))
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, false)(PersistentTenantKind.GSP)

      verifyValidationFormResult(result, expected)

    }

    "succeed on correct form 1.0 only schema on tenant kind GSP" in {
      val riskAnalysis = SpecData.validOnlySchemaRiskAnalysis1_0

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(SingleAnswerSeed("purpose", Some("INSTITUTIONAL"))),
        multiAnswers = Seq.empty
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.GSP)

      verifyValidationFormResult(result, expected)

    }

    "fail if version does not exists" in {
      val riskAnalysis = SpecData.validRiskAnalysis2_0_Pa.copy(version = "9999.0")

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, false)(PersistentTenantKind.PA)

      verifyValidationFailure(result, _.contains(UnexpectedTemplateVersion("9999.0")) shouldBe true)

    }

    "fail if a provided answer depends on a missing field" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "2.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("YES"),
          "usesThirdPartyPersonalData" -> Nil,
          "usesConfidentialData"       -> List("YES")
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, false)(PersistentTenantKind.PA)

      verifyValidationFailure(result, _.contains(MissingExpectedField("doneDpia")) shouldBe true)
      verifyValidationFailure(result, _.contains(MissingExpectedField("policyProvided")) shouldBe true)
      verifyValidationFailure(result, _.contains(MissingExpectedField("deliveryMethod")) shouldBe true)
    }

    "succeed only schema (fail if a provided answer depends on a missing field)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "2.0",
        answers = Map(
          "purpose"                  -> List("INSTITUTIONAL"),
          "institutionalPurpose"     -> List("institutionalPurpose"),
          "otherPurpose"             -> List("otherPurpose"),
          "legalBasisPublicInterest" -> List("RULE_OF_LAW")
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PA)

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", Some("INSTITUTIONAL")),
          SingleAnswerSeed("institutionalPurpose", Some("institutionalPurpose")),
          SingleAnswerSeed("otherPurpose", Some("otherPurpose")),
          SingleAnswerSeed("legalBasisPublicInterest", Some("RULE_OF_LAW"))
        ),
        multiAnswers = Seq.empty
      )

      verifyValidationFormResult(result, expected)
    }

    "fail if a provided answer depends on an existing field with an unexpected value" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "2.0",
        answers = Map(
          "purpose"                  -> List("INSTITUTIONAL"),
          "institutionalPurpose"     -> List("institutionalPurpose"),
          "otherPurpose"             -> List("otherPurpose"),
          "legalBasisPublicInterest" -> List("RULE_OF_LAW")
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, false)(PersistentTenantKind.PA)

      verifyValidationFailure(
        result,
        _.contains(UnexpectedFieldValueByDependency("purpose", "otherPurpose", "OTHER")) shouldBe true
      )
    }

    "succeed only schema (complete validation should fail because provided answer depends on an existing field with an unexpected value)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = Map(
          "purpose"                  -> List("INSTITUTIONAL"),
          "institutionalPurpose"     -> List("institutionalPurpose"),
          "otherPurpose"             -> List("otherPurpose"),
          "legalBasisPublicInterest" -> List("RULE_OF_LAW")
        )
      )

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswerSeed("purpose", "INSTITUTIONAL".some),
          SingleAnswerSeed("institutionalPurpose", "institutionalPurpose".some),
          SingleAnswerSeed("otherPurpose", "otherPurpose".some),
          SingleAnswerSeed("legalBasisPublicInterest", "RULE_OF_LAW".some)
        ),
        multiAnswers = Seq.empty
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PRIVATE)

      verifyValidationFormResult(result, expected)

    }

    "fail on missing expected answer (answer tree is not complete)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "2.0",
        answers = Map(
          "purpose"                    -> List("purpose"),
          "usesPersonalData"           -> List("NO"),
          "usesThirdPartyPersonalData" -> List("YES"),
          "usesConfidentialData"       -> Nil,
          "securedDataAccess"          -> Nil
        )
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, false)(PersistentTenantKind.PA)

      verifyValidationFailure(
        result,
        err =>
          (err.contains(MissingExpectedField("personalDataTypes")) &&
            err.contains(MissingExpectedField("legalBasis")) &&
            err.contains(MissingExpectedField("knowsDataQuantity")) &&
            err.contains(MissingExpectedField("deliveryMethod")) &&
            err.contains(MissingExpectedField("policyProvided")) &&
            err.contains(MissingExpectedField("confirmPricipleIntegrityAndDiscretion")) &&
            err.contains(MissingExpectedField("doneDpia")) &&
            err.contains(MissingExpectedField("purposePursuit")) &&
            err.contains(MissingExpectedField("usesThirdPartyData")) &&
            err.contains(MissingExpectedField("dataRetentionPeriod")) &&
            err.contains(MissingExpectedField("declarationConfirmGDPR"))) shouldBe true
      )

    }

    "succeed only schema (complete validation should fail because missing expected answer, as tree is not complete)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "2.0",
        answers = Map("purpose" -> List("INSTITUTIONAL"), "usesConfidentialData" -> Nil, "securedDataAccess" -> Nil)
      )

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PA)

      val expected = RiskAnalysisFormSeed(
        version = riskAnalysis.version,
        singleAnswers = Seq(SingleAnswerSeed("purpose", "INSTITUTIONAL".some)),
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

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PRIVATE)

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

      val result: ValidationResult[RiskAnalysisFormSeed] =
        RiskAnalysisValidation.validate(riskAnalysis, true)(PersistentTenantKind.PRIVATE)

      verifyValidationFailure(
        result,
        _.contains(UnexpectedFieldValue("usesPersonalData", Option(Set("YES", "NO")))) shouldBe true
      )
      verifyValidationFailure(
        result,
        _.contains(UnexpectedFieldValue("purpose", Option(Set("INSTITUTIONAL", "OTHER")))) shouldBe true
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
