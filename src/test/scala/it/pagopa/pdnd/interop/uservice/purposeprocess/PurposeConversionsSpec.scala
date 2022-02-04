package it.pagopa.pdnd.interop.uservice.purposeprocess

import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{
  Purpose => DependencyPurpose,
  PurposeVersion => DependencyPurposeVersion,
  PurposeVersionDocument => DependencyPurposeVersionDocument,
  PurposeVersionState => DependencyPurposeVersionState
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.converters.purposemanagement.PurposeConverter
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{
  Purpose,
  PurposeVersion,
  PurposeVersionDocument,
  PurposeVersionState
}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

import java.time.OffsetDateTime
import java.util.UUID

class PurposeConversionsSpec extends AnyWordSpecLike {
  "Dependency Purpose" should {
    "be converted to Process Purpose" in {
      val dependencyPurpose = DependencyPurpose(
        id = UUID.randomUUID(),
        eserviceId = UUID.randomUUID(),
        consumerId = UUID.randomUUID(),
        versions = Seq(
          DependencyPurposeVersion(
            id = UUID.randomUUID(),
            state = DependencyPurposeVersionState.DRAFT,
            createdAt = OffsetDateTime.now(),
            updatedAt = Some(OffsetDateTime.now()),
            expectedApprovalDate = Some(OffsetDateTime.now()),
            dailyCalls = 100,
            riskAnalysis = Some(
              DependencyPurposeVersionDocument(
                id = UUID.randomUUID(),
                contentType = "content-type",
                path = "a/path",
                createdAt = OffsetDateTime.now()
              )
            )
          )
        ),
        suspendedByConsumer = Some(true),
        suspendedByProducer = Some(false),
        title = "A title",
        description = Some("A description"),
        riskAnalysisForm = SpecData.validManagementRiskAnalysis,
        createdAt = OffsetDateTime.now(),
        updatedAt = Some(OffsetDateTime.now())
      )

      val expectedPurpose = Purpose(
        id = dependencyPurpose.id,
        eserviceId = dependencyPurpose.eserviceId,
        consumerId = dependencyPurpose.consumerId,
        versions = Seq(
          PurposeVersion(
            id = dependencyPurpose.versions.head.id,
            state = PurposeVersionState.DRAFT,
            createdAt = dependencyPurpose.versions.head.createdAt,
            updatedAt = dependencyPurpose.versions.head.updatedAt,
            expectedApprovalDate = dependencyPurpose.versions.head.expectedApprovalDate,
            riskAnalysis = Some(
              PurposeVersionDocument(
                id = dependencyPurpose.versions.head.riskAnalysis.get.id,
                contentType = "content-type",
                createdAt = dependencyPurpose.versions.head.riskAnalysis.get.createdAt
              )
            )
          )
        ),
        suspendedByConsumer = Some(true),
        suspendedByProducer = Some(false),
        title = "A title",
        description = Some("A description"),
        riskAnalysisForm = SpecData.validRiskAnalysis,
        createdAt = dependencyPurpose.createdAt,
        updatedAt = dependencyPurpose.updatedAt
      )

      val purpose = PurposeConverter.dependencyToApi(dependencyPurpose)

      purpose shouldBe Right(expectedPurpose)
    }
  }
}
