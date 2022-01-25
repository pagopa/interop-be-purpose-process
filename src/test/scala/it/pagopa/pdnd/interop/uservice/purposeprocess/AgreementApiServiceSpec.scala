package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import it.pagopa.pdnd.interop.commons.jwt.service.JWTReader
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.StateChangeDetails
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.{PurposeApiMarshallerImpl, PurposeApiServiceImpl}
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.{
  PurposeApi,
  PurposeApiMarshaller,
  ConsumerApiMarshaller,
  HealthApi
}
import it.pagopa.pdnd.interop.uservice.purposeprocess.model._
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.PurposeManagementService.purposeStateToApi
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.{
  PurposeManagementService,
  AttributeManagementService,
  CatalogManagementService
}
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model.EServiceDescriptor
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.{model => CatalogManagementDependency}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json.RootJsonFormat

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import it.pagopa.pdnd.interop.commons.utils.SprayCommonFormats.{offsetDateTimeFormat, uuidFormat}

class PurposeApiServiceSpec extends AnyWordSpecLike with MockFactory with SpecHelper with ScalatestRouteTest {

  val consumerApiMarshaller: ConsumerApiMarshaller               = new ConsumerApiMarshallerImpl
  val purposeApiMarshaller: PurposeApiMarshaller                 = new PurposeApiMarshallerImpl
  val mockHealthApi: HealthApi                                   = mock[HealthApi]
  val mockPurposeApi: PurposeApi                                 = mock[PurposeApi]
  val mockPartyManagementService: PartyManagementService         = mock[PartyManagementService]
  val mockPurposeManagementService: PurposeManagementService     = mock[PurposeManagementService]
  val mockCatalogManagementService: CatalogManagementService     = mock[CatalogManagementService]
  val mockAttributeManagementService: AttributeManagementService = mock[AttributeManagementService]
  val mockJWTReader: JWTReader                                   = mock[JWTReader]

  import consumerApiMarshaller._

  val service = new PurposeApiServiceImpl(
    mockPurposeManagementService,
    mockCatalogManagementService,
    mockPartyManagementService,
    mockAttributeManagementService,
    mockJWTReader
  )(ExecutionContext.global)

  implicit val context: Seq[(String, String)] = Seq("bearer" -> Common.bearerToken)

  "Purpose Activation" should {
    "succeed on pending purpose" in {
      val pendingPurpose = TestDataOne.purpose.copy(state = PurposeManagementDependency.PurposeState.PENDING)
      val eService = TestDataOne.eService.copy(descriptors =
        Seq(
          EServiceDescriptor(
            pendingPurpose.descriptorId,
            "1",
            None,
            Seq.empty,
            0,
            None,
            Seq.empty,
            CatalogManagementDependency.EServiceDescriptorState.PUBLISHED
          )
        )
      )

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataOne.id.toString)
        .once()
        .returns(Future.successful(pendingPurpose))

      (
        mockPurposeManagementService
          .getPurposes(_: String)(
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[PurposeManagementDependency.PurposeState]
          )
        )
        .expects(
          Common.bearerToken,
          Some(TestDataOne.producerId.toString),
          Some(pendingPurpose.consumerId.toString),
          Some(eService.id.toString),
          Some(TestDataOne.descriptorId.toString),
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .once()
        .returns(Future.successful(Seq.empty))

      (mockPartyManagementService
        .getPartyAttributes(_: String)(_: UUID))
        .expects(Common.bearerToken, pendingPurpose.consumerId)
        .once()
        .returns(Future.successful(Seq.empty))

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, eService.id)
        .once()
        .returns(Future.successful(eService))

      (mockPurposeManagementService
        .activateById(_: String)(_: String, _: StateChangeDetails))
        .expects(
          Common.bearerToken,
          TestDataOne.id.toString,
          StateChangeDetails(changedBy = Some(PurposeManagementDependency.ChangedBy.PRODUCER))
        )
        .once()
        .returns(Future.successful(pendingPurpose))

      Get() ~> service.activatePurpose(TestDataOne.id.toString, TestDataOne.producerId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
      }
    }

    "succeed on suspended purpose" in {
      val suspendedPurpose =
        TestDataOne.purpose.copy(state = PurposeManagementDependency.PurposeState.SUSPENDED)
      val eService = TestDataOne.eService.copy(descriptors =
        Seq(
          EServiceDescriptor(
            suspendedPurpose.descriptorId,
            "1",
            None,
            Seq.empty,
            0,
            None,
            Seq.empty,
            CatalogManagementDependency.EServiceDescriptorState.PUBLISHED
          )
        )
      )

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataOne.id.toString)
        .once()
        .returns(Future.successful(suspendedPurpose))

      (
        mockPurposeManagementService
          .getPurposes(_: String)(
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[PurposeManagementDependency.PurposeState]
          )
        )
        .expects(
          Common.bearerToken,
          Some(TestDataOne.producerId.toString),
          Some(suspendedPurpose.consumerId.toString),
          Some(eService.id.toString),
          Some(TestDataOne.descriptorId.toString),
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .once()
        .returns(Future.successful(Seq.empty))

      (mockPartyManagementService
        .getPartyAttributes(_: String)(_: UUID))
        .expects(Common.bearerToken, suspendedPurpose.consumerId)
        .once()
        .returns(Future.successful(Seq.empty))

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, eService.id)
        .once()
        .returns(Future.successful(eService))

      (mockPurposeManagementService
        .activateById(_: String)(_: String, _: StateChangeDetails))
        .expects(
          Common.bearerToken,
          TestDataOne.id.toString,
          StateChangeDetails(changedBy = Some(PurposeManagementDependency.ChangedBy.PRODUCER))
        )
        .once()
        .returns(Future.successful(suspendedPurpose))

      Get() ~> service.activatePurpose(TestDataOne.id.toString, TestDataOne.producerId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
      }
    }

    "fail if missing authorization header" in {
      val contexts: Seq[(String, String)] = Seq.empty[(String, String)]
      Get() ~> service.activatePurpose(TestDataOne.id.toString, TestDataOne.producerId.toString)(
        contexts,
        toEntityMarshallerProblem
      ) ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "fail if an active purpose already exists" in {
      val currentPurpose = TestDataOne.purpose.copy(state = PurposeManagementDependency.PurposeState.SUSPENDED)
      val activePurpose =
        TestDataOne.purpose.copy(id = UUID.randomUUID(), state = PurposeManagementDependency.PurposeState.ACTIVE)

      val eService = TestDataOne.eService.copy(descriptors =
        Seq(
          EServiceDescriptor(
            currentPurpose.descriptorId,
            "1",
            None,
            Seq.empty,
            0,
            None,
            Seq.empty,
            CatalogManagementDependency.EServiceDescriptorState.PUBLISHED
          )
        )
      )

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataOne.id.toString)
        .once()
        .returns(Future.successful(currentPurpose))

      (
        mockPurposeManagementService
          .getPurposes(_: String)(
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[PurposeManagementDependency.PurposeState]
          )
        )
        .expects(
          Common.bearerToken,
          Some(TestDataOne.producerId.toString),
          Some(currentPurpose.consumerId.toString),
          Some(eService.id.toString),
          Some(TestDataOne.descriptorId.toString),
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .once()
        .returns(Future.successful(Seq(activePurpose)))

      Get() ~> service.activatePurpose(TestDataOne.id.toString, TestDataOne.producerId.toString) ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "fail if purpose is not Pending or Suspended" in {
      val currentPurpose = TestDataOne.purpose.copy(state = PurposeManagementDependency.PurposeState.ACTIVE)

      val eService = TestDataOne.eService.copy(descriptors =
        Seq(
          EServiceDescriptor(
            currentPurpose.descriptorId,
            "1",
            None,
            Seq.empty,
            0,
            None,
            Seq.empty,
            CatalogManagementDependency.EServiceDescriptorState.PUBLISHED
          )
        )
      )

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataOne.id.toString)
        .once()
        .returns(Future.successful(currentPurpose))

      (
        mockPurposeManagementService
          .getPurposes(_: String)(
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[PurposeManagementDependency.PurposeState]
          )
        )
        .expects(
          Common.bearerToken,
          Some(TestDataOne.producerId.toString),
          Some(currentPurpose.consumerId.toString),
          Some(eService.id.toString),
          Some(TestDataOne.descriptorId.toString),
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .once()
        .returns(Future.successful(Seq.empty))

      Get() ~> service.activatePurpose(TestDataOne.id.toString, TestDataOne.producerId.toString) ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "fail if descriptor is not Published" in {
      val currentPurpose = TestDataOne.purpose.copy(state = PurposeManagementDependency.PurposeState.SUSPENDED)

      val eService = TestDataOne.eService.copy(descriptors =
        Seq(
          EServiceDescriptor(
            currentPurpose.descriptorId,
            "1",
            None,
            Seq.empty,
            0,
            None,
            Seq.empty,
            CatalogManagementDependency.EServiceDescriptorState.DEPRECATED
          )
        )
      )

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataOne.id.toString)
        .once()
        .returns(Future.successful(currentPurpose))

      (
        mockPurposeManagementService
          .getPurposes(_: String)(
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[String],
            _: Option[PurposeManagementDependency.PurposeState]
          )
        )
        .expects(
          Common.bearerToken,
          Some(TestDataOne.producerId.toString),
          Some(currentPurpose.consumerId.toString),
          Some(eService.id.toString),
          Some(TestDataOne.descriptorId.toString),
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .once()
        .returns(Future.successful(Seq.empty))

      (mockPartyManagementService
        .getPartyAttributes(_: String)(_: UUID))
        .expects(Common.bearerToken, currentPurpose.consumerId)
        .once()
        .returns(Future.successful(Seq.empty))

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, eService.id)
        .once()
        .returns(Future.successful(eService))

      Get() ~> service.activatePurpose(TestDataOne.id.toString, TestDataOne.producerId.toString) ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

  }

  "Purpose Suspension" should {
    "succeed on active purpose" in {
      val activePurpose = TestDataOne.purpose.copy(state = PurposeManagementDependency.PurposeState.ACTIVE)

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataOne.id.toString)
        .once()
        .returns(Future.successful(activePurpose))

      (mockPurposeManagementService
        .suspendById(_: String)(_: String, _: StateChangeDetails))
        .expects(
          Common.bearerToken,
          TestDataOne.id.toString,
          StateChangeDetails(Some(PurposeManagementDependency.ChangedBy.PRODUCER))
        )
        .once()
        .returns(Future.successful(activePurpose))

      Get() ~> service.suspendPurpose(TestDataOne.id.toString, TestDataOne.producerId.toString) ~> check {
        status shouldEqual StatusCodes.NoContent
      }
    }

    "fail if missing authorization header" in {
      val contexts: Seq[(String, String)] = Seq.empty[(String, String)]
      Get() ~> service.suspendPurpose(TestDataOne.id.toString, TestDataOne.producerId.toString)(
        contexts,
        toEntityMarshallerProblem
      ) ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "fail if purpose is not Active" in {
      val currentPurpose = TestDataOne.purpose.copy(state = PurposeManagementDependency.PurposeState.SUSPENDED)

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataOne.id.toString)
        .once()
        .returns(Future.successful(currentPurpose))

      Get() ~> service.suspendPurpose(TestDataOne.id.toString, TestDataOne.producerId.toString) ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  "Purpose GET" should {
    "retrieves an purpose" in {

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

      (mockPurposeManagementService
        .getPurposeById(_: String)(_: String))
        .expects(Common.bearerToken, TestDataSeven.purposeId.toString)
        .once()
        .returns(Future.successful(TestDataSeven.purpose))

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataSeven.eservice.id)
        .once()
        .returns(Future.successful(TestDataSeven.eservice))

      (mockPartyManagementService
        .getOrganization(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataSeven.producerId)
        .once()
        .returns(Future.successful(TestDataSeven.producer))

      (mockPartyManagementService
        .getOrganization(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataSeven.consumerId)
        .once()
        .returns(Future.successful(TestDataSeven.consumer))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, TestDataSeven.eservice.attributes.verified(0).single.get.id)
        .once()
        .returns(Future.successful(ClientAttributes.verifiedAttributeId1))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, TestDataSeven.eservice.attributes.verified(1).single.get.id)
        .once()
        .returns(Future.successful(ClientAttributes.verifiedAttributeId2))

      import purposeApiMarshaller._
      import spray.json.DefaultJsonProtocol._

      implicit def organizationJsonFormat: RootJsonFormat[Organization]           = jsonFormat2(Organization)
      implicit def activeDescriptorJsonFormat: RootJsonFormat[ActiveDescriptor]   = jsonFormat3(ActiveDescriptor)
      implicit def eServiceJsonFormat: RootJsonFormat[EService]                   = jsonFormat4(EService)
      implicit def attributeJsonFormat: RootJsonFormat[Attribute]                 = jsonFormat9(Attribute)
      implicit def purposeAttributesJsonFormat: RootJsonFormat[PurposeAttributes] = jsonFormat2(PurposeAttributes)
      implicit def purposeJsonFormat: RootJsonFormat[Purpose]                     = jsonFormat9(Purpose)
      import SprayJsonSupport.sprayJsonUnmarshaller

      implicit def fromEntityUnmarshallerPurpose: FromEntityUnmarshaller[Purpose] =
        sprayJsonUnmarshaller[Purpose]

      val expected = Purpose(
        id = TestDataSeven.purposeId,
        producer = Organization(id = TestDataSeven.producer.institutionId, name = TestDataSeven.producer.description),
        consumer = Organization(id = TestDataSeven.consumer.institutionId, name = TestDataSeven.consumer.description),
        eservice = EService(
          id = TestDataSeven.eservice.id,
          name = TestDataSeven.eservice.name,
          version = TestDataSeven.eservice.descriptors(0).version,
          activeDescriptor = None
        ),
        eserviceDescriptorId = TestDataSeven.eservice.descriptors(0).id,
        state = purposeStateToApi(TestDataSeven.purpose.state),
        suspendedByConsumer = None,
        suspendedByProducer = None,
        attributes = Seq(
          PurposeAttributes(
            single = Some(
              Attribute(
                id = UUID.fromString(ClientAttributes.verifiedAttributeId1.id),
                code = ClientAttributes.verifiedAttributeId1.code,
                description = ClientAttributes.verifiedAttributeId1.description,
                origin = ClientAttributes.verifiedAttributeId1.origin,
                name = ClientAttributes.verifiedAttributeId1.name,
                explicitAttributeVerification =
                  Some(TestDataSeven.eservice.attributes.verified(0).single.get.explicitAttributeVerification),
                verified = TestDataSeven.purpose.verifiedAttributes(0).verified,
                verificationDate = TestDataSeven.purpose.verifiedAttributes(0).verificationDate,
                validityTimespan = TestDataSeven.purpose.verifiedAttributes(0).validityTimespan
              )
            ),
            group = None
          ),
          PurposeAttributes(
            single = Some(
              Attribute(
                id = UUID.fromString(ClientAttributes.verifiedAttributeId2.id),
                code = ClientAttributes.verifiedAttributeId2.code,
                description = ClientAttributes.verifiedAttributeId2.description,
                origin = ClientAttributes.verifiedAttributeId2.origin,
                name = ClientAttributes.verifiedAttributeId2.name,
                explicitAttributeVerification =
                  Some(TestDataSeven.eservice.attributes.verified(1).single.get.explicitAttributeVerification),
                verified = TestDataSeven.purpose.verifiedAttributes(1).verified,
                verificationDate = TestDataSeven.purpose.verifiedAttributes(1).verificationDate,
                validityTimespan = TestDataSeven.purpose.verifiedAttributes(1).validityTimespan
              )
            ),
            group = None
          )
        )
      )

      Get() ~> service.getPurposeById(TestDataSeven.purposeId.toString) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Purpose] shouldEqual expected
      }
    }

  }

}
