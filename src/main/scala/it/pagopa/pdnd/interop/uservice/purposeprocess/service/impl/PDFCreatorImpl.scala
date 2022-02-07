package it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl

import com.openhtmltopdf.util.XRLog
import it.pagopa.pdnd.interop.commons.files.service.PDFManager
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.RiskAnalysisForm
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.PDFCreator

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.Future
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.Try

object PDFCreatorImpl extends PDFCreator with PDFManager {

  // Suppressing openhtmltopdf log
  XRLog.listRegisteredLoggers.asScala.foreach((logger: String) =>
    XRLog.setLevel(logger, java.util.logging.Level.SEVERE)
  )

  override def createDocument(template: String, riskAnalysisForm: RiskAnalysisForm, dailyCalls: Int): Future[File] =
    Future.fromTry {
      for {
        file <- createTempFile
        data = setupData(riskAnalysisForm, dailyCalls)
        pdf <- getPDFAsFile(file.toPath, template, data)
      } yield pdf

    }

  private def createTempFile: Try[File] = {
    Try {
      val fileTimestamp: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
      File.createTempFile(s"${fileTimestamp}_${UUID.randomUUID().toString}_risk_analysis.", ".pdf")
    }
  }

  // TODO This implementation depends on the template (to be defined)
  private def setupData(riskAnalysisForm: RiskAnalysisForm, dailyCalls: Int): Map[String, String] = {
    Map("dailyCalls" -> dailyCalls.toString) ++
      riskAnalysisForm.singleAnswers.map(answer => answer.key -> answer.value.getOrElse("")).toMap ++
      riskAnalysisForm.multiAnswers
        .flatMap(answer => answer.values.map(value => s"${answer.key}_$value" -> value))
        .toMap
  }

  //  private def setupData(riskAnalysisForm: RiskAnalysisForm, dailyCalls: Int): Map[String, String] =
//    riskAnalysisForm.answers match {
//      case RiskAnalysisFormAnswers(
//            purpose,
//            usesPersonalData,
//            usesThirdPartyPersonalData,
//            usesConfidentialData,
//            securedDataAccess,
//            legalBasis,
//            legalObligationReference,
//            publicInterestReference,
//            knowsAccessedDataCategories,
//            accessDataArt9Gdpr,
//            accessUnderageData,
//            knowsDataQuantity,
//            dataQuantity,
//            deliveryMethod,
//            doneDpia,
//            definedDataRetentionPeriod,
//            purposePursuit,
//            checkedExistenceMereCorrectnessInteropCatalogue,
//            checkedAllDataNeeded,
//            checkedExistenceMinimalDataInteropCatalogue
//          ) =>
//        Map(
//          ValidationRules.PURPOSE            -> purpose,
//          ValidationRules.USES_PERSONAL_DATA -> usesPersonalData.toString,
//          "dailyCalls"                       -> dailyCalls.toString
//        ) ++
//          singleAnswerToMap(ValidationRules.USES_THIRD_PARTY_PERSONAL_DATA, usesThirdPartyPersonalData) ++
//          singleAnswerToMap(ValidationRules.USES_CONFIDENTIAL_DATA, usesConfidentialData) ++
//          singleAnswerToMap(ValidationRules.SECURED_DATA_ACCESS, securedDataAccess) ++
//          singleAnswerToMap(ValidationRules.LEGAL_OBLIGATION_REFERENCE, legalObligationReference) ++
//          singleAnswerToMap(ValidationRules.PUBLIC_INTEREST_REFERENCE, publicInterestReference) ++
//          singleAnswerToMap(ValidationRules.KNOWS_ACCESSED_DATA_CATEGORIES, knowsAccessedDataCategories) ++
//          singleAnswerToMap(ValidationRules.ACCESS_DATA_ART9_GDPR, accessDataArt9Gdpr) ++
//          singleAnswerToMap(ValidationRules.ACCESS_UNDERAGE_DATA, accessUnderageData) ++
//          singleAnswerToMap(ValidationRules.KNOWS_DATA_QUANTITY, knowsDataQuantity) ++
//          singleAnswerToMap(ValidationRules.DATA_QUANTITY, dataQuantity) ++
//          singleAnswerToMap(ValidationRules.DELIVERY_METHOD, deliveryMethod) ++
//          singleAnswerToMap(ValidationRules.DONE_DPIA, doneDpia) ++
//          singleAnswerToMap(ValidationRules.PURPOSE_PURSUIT, definedDataRetentionPeriod) ++
//          singleAnswerToMap(ValidationRules.PURPOSE_PURSUIT, purposePursuit) ++
//          singleAnswerToMap(ValidationRules.CHECKED_ALL_DATA_NEEDED, checkedAllDataNeeded) ++
//          singleAnswerToMap(
//            ValidationRules.CHECKED_EXISTENCE_MINIMAL_DATA_INTEROP_CATALOGUE,
//            checkedExistenceMinimalDataInteropCatalogue
//          ) ++
//          multiAnswerToMap(ValidationRules.LEGAL_BASIS, legalBasis) ++
//          multiAnswerToMap(
//            ValidationRules.CHECKED_EXISTENCE_MERE_CORRECTNESS_INTEROP_CATALOGUE,
//            checkedExistenceMereCorrectnessInteropCatalogue
//          )
//    }
//
//  def multiAnswerToMap[T](label: String, optSeq: Option[Seq[T]]): Map[String, String] =
//    optSeq
//      .map(_.map(value => s"${label}_$value" -> value.toString).toMap)
//      .getOrElse(Map.empty)
//
//  def singleAnswerToMap[T](label: String, opt: Option[T]): Map[String, String] =
//    opt.map(value => Map(label -> value.toString)).getOrElse(Map.empty)

}
