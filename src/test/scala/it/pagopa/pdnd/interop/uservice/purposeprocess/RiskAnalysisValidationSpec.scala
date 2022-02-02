package it.pagopa.pdnd.interop.uservice.purposeprocess

import cats.implicits._
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  RiskAnalysisForm => DepRiskAnalysisForm,
  RiskAnalysisSingleAnswer => SingleAnswer
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.RiskAnalysisValidation
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.RiskAnalysisValidation.{
  DependencyNotFound,
  MissingExpectedField,
  UnexpectedFieldValue,
  ValidationResult
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{
  FormUsesConfidentialDataAnswers,
  FormUsesPersonalDataAnswers,
  FormUsesThirdPartyPersonalDataAnswers,
  RiskAnalysisForm,
  RiskAnalysisFormAnswers
}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

class RiskAnalysisValidationSpec extends AnyWordSpecLike {
  "Risk Analysis Validation" should {
    "succeed on correct form" in {

      val riskAnalysis = SpecData.validRiskAnalysis

      val expected = DepRiskAnalysisForm(
        version = riskAnalysis.version,
        singleAnswers = Seq(
          SingleAnswer("purpose", Some(riskAnalysis.answers.purpose)),
          SingleAnswer("usesPersonalData", Some(riskAnalysis.answers.usesPersonalData.toString))
        ),
        multiAnswers = Seq.empty
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      result shouldBe expected.validNec
    }

    // TODO
    "succeed if not required field is missing" in {}

    "fail if an existing answer depends on a missing field" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = RiskAnalysisFormAnswers(
          purpose = "purpose",
          usesPersonalData = FormUsesPersonalDataAnswers.usesPersonalDataYes,
          usesThirdPartyPersonalData = None,
          usesConfidentialData = Some(FormUsesConfidentialDataAnswers.usesConfidentialDataYes)
        )
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      result shouldBe DependencyNotFound("usesThirdPartyPersonalData").invalidNec
    }

    "fail if an existing answer depends on a field with an unexpected value" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = RiskAnalysisFormAnswers(
          purpose = "purpose",
          usesPersonalData = FormUsesPersonalDataAnswers.usesPersonalDataNo,
          usesThirdPartyPersonalData = Some(FormUsesThirdPartyPersonalDataAnswers.usesThirdPartyPersonalDataNo),
          usesConfidentialData = Some(FormUsesConfidentialDataAnswers.usesConfidentialDataYes)
        )
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      result shouldBe UnexpectedFieldValue("usesThirdPartyPersonalData").invalidNec
    }

    // TODO
    "fail on missing expected answer (answer tree is not complete)" in {
      val riskAnalysis = RiskAnalysisForm(
        version = "1.0",
        answers = RiskAnalysisFormAnswers(
          purpose = "purpose",
          usesPersonalData = FormUsesPersonalDataAnswers.usesPersonalDataNo,
          usesThirdPartyPersonalData = Some(FormUsesThirdPartyPersonalDataAnswers.usesThirdPartyPersonalDataYes),
          usesConfidentialData = None
        )
      )

      val result: ValidationResult[DepRiskAnalysisForm] = RiskAnalysisValidation.validate(riskAnalysis)

      result shouldBe MissingExpectedField("usesConfidentialData").invalidNec
    }
  }
}
