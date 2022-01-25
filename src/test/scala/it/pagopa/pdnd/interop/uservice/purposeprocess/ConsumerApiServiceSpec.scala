package it.pagopa.pdnd.interop.uservice.purposeprocess

import akka.actor.ActorSystem
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.directives.{AuthenticationDirective, SecurityDirectives}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshal}
import it.pagopa.pdnd.interop.commons.jwt.service.JWTReader
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.{model => PurposeManagementDependency}
import it.pagopa.pdnd.interop.commons.utils.SprayCommonFormats.{offsetDateTimeFormat, uuidFormat}
import it.pagopa.pdnd.interop.commons.utils.AkkaUtils.Authenticator
import it.pagopa.pdnd.interop.uservice.purposeprocess.api.{PurposeApi, ConsumerApi, ConsumerApiMarshaller, HealthApi}
import it.pagopa.pdnd.interop.uservice.purposeprocess.common.system.executionContext
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Attribute, Attributes}
import it.pagopa.pdnd.interop.uservice.purposeprocess.server.Controller
import it.pagopa.pdnd.interop.uservice.purposeprocess.service._
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpecLike
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.util.UUID
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future}

class ConsumerApiServiceSpec
    extends ScalaTestWithActorTestKit
    with MockFactory
    with AnyWordSpecLike
    with SprayJsonSupport
    with DefaultJsonProtocol
    with SpecHelper {

  implicit val testSystem: ActorSystem = system.classicSystem

  val consumerApiMarshaller: ConsumerApiMarshaller               = new ConsumerApiMarshallerImpl
  val mockHealthApi: HealthApi                                   = mock[HealthApi]
  val mockPurposeApi: PurposeApi                                 = mock[PurposeApi]
  val mockPartyManagementService: PartyManagementService         = mock[PartyManagementService]
  val mockPurposeManagementService: PurposeManagementService     = mock[PurposeManagementService]
  val mockCatalogManagementService: CatalogManagementService     = mock[CatalogManagementService]
  val mockAttributeManagementService: AttributeManagementService = mock[AttributeManagementService]
  val mockJWTReader: JWTReader                                   = mock[JWTReader]

  var controller: Option[Controller]                 = None
  var bindServer: Option[Future[Http.ServerBinding]] = None

  val wrappingDirective: AuthenticationDirective[Seq[(String, String)]] =
    SecurityDirectives.authenticateOAuth2("SecurityRealm", Authenticator)

  override def beforeAll(): Unit = {

    val consumerApi = new ConsumerApi(
      new ConsumerApiServiceImpl(
        purposeManagementService = mockPurposeManagementService,
        partyManagementService = mockPartyManagementService,
        catalogManagementService = mockCatalogManagementService,
        attributeManagementService = mockAttributeManagementService,
        jwtReader = mockJWTReader
      ),
      consumerApiMarshaller,
      wrappingDirective
    )

    controller = Some(new Controller(purpose = mockPurposeApi, health = mockHealthApi, consumer = consumerApi))

    controller foreach { controller =>
      bindServer = Some(
        Http()
          .newServerAt("0.0.0.0", 8088)
          .bind(controller.routes)
      )

      Await.result(bindServer.get, 100.seconds)
    }
  }

  override def afterAll(): Unit = {
    bindServer.foreach(_.foreach(_.unbind()))
  }

  "Processing a consumer request" should {

    "retrieve all attributes owned by a customer (customer with all kind attributes)" in {

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

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
          None,
          Some(Common.consumerId),
          None,
          None,
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .returns(Future.successful(Seq(TestDataOne.purpose, TestDataTwo.purpose)))

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataOne.eserviceId)
        .returns(Future.successful(TestDataOne.eService))
        .once()

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataTwo.eserviceId)
        .returns(Future.successful(TestDataTwo.eService))
        .once()

      (mockPartyManagementService
        .getPartyAttributes(_: String)(_: UUID))
        .expects(Common.bearerToken, UUID.fromString(Common.consumerId))
        .returns(Future.successful(Seq(Common.certifiedAttribute)))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.certifiedAttribute)
        .returns(Future.successful[ClientAttribute](ClientAttributes.certifiedAttribute))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.verifiedAttributeId1)
        .returns(Future.successful[ClientAttribute](ClientAttributes.verifiedAttributeId1))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.verifiedAttributeId2)
        .returns(Future.successful[ClientAttribute](ClientAttributes.verifiedAttributeId2))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.verifiedAttributeId3)
        .returns(Future.successful[ClientAttribute](ClientAttributes.verifiedAttributeId3))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.declaredAttributeId1)
        .returns(Future.successful[ClientAttribute](ClientAttributes.declaredAttributeId1))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.declaredAttributeId2)
        .returns(Future.successful[ClientAttribute](ClientAttributes.declaredAttributeId2))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.declaredAttributeId3)
        .returns(Future.successful[ClientAttribute](ClientAttributes.declaredAttributeId3))

      val response =
        request(data = emptyData, path = s"consumers/${Common.consumerId}/attributes", verb = HttpMethods.GET)

      implicit def attributeJsonFormat: RootJsonFormat[Attribute] = jsonFormat9(Attribute)

      implicit val fromEntityUnmarshallerAttributes: FromEntityUnmarshaller[Attributes] =
        sprayJsonUnmarshaller[Attributes](jsonFormat3(Attributes))

      val body = Await.result(Unmarshal(response.entity).to[Attributes], Duration.Inf)

      body.certified.count(
        _ == AttributeManagementService.toApi(ClientAttributes.certifiedAttribute).futureValue
      ) shouldBe 1

      body.declared.toSet shouldBe Set(
        AttributeManagementService.toApi(ClientAttributes.declaredAttributeId1).futureValue,
        AttributeManagementService.toApi(ClientAttributes.declaredAttributeId2).futureValue,
        AttributeManagementService.toApi(ClientAttributes.declaredAttributeId3).futureValue
      )

      body.verified.toSet shouldBe Set(
        AttributeManagementService.toApi(ClientAttributes.verifiedAttributeId1).futureValue,
        AttributeManagementService.toApi(ClientAttributes.verifiedAttributeId2).futureValue,
        AttributeManagementService.toApi(ClientAttributes.verifiedAttributeId3).futureValue
      )

    }

    "retrieve all attributes owned by a customer (customer without verified attributes)" in {

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

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
          None,
          Some(Common.consumerId),
          None,
          None,
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .returns(Future.successful(Seq(TestDataOne.purpose, TestDataThree.purpose)))

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataOne.eserviceId)
        .returns(Future.successful(TestDataOne.eService))
        .once()

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataThree.eserviceId)
        .returns(Future.successful(TestDataThree.eService))
        .once()

      (mockPartyManagementService
        .getPartyAttributes(_: String)(_: UUID))
        .expects(Common.bearerToken, UUID.fromString(Common.consumerId))
        .returns(Future.successful(Seq(Common.certifiedAttribute)))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.certifiedAttribute)
        .returns(Future.successful[ClientAttribute](ClientAttributes.certifiedAttribute))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.declaredAttributeId1)
        .returns(Future.successful[ClientAttribute](ClientAttributes.declaredAttributeId1))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.declaredAttributeId2)
        .returns(Future.successful[ClientAttribute](ClientAttributes.declaredAttributeId2))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.declaredAttributeId3)
        .returns(Future.successful[ClientAttribute](ClientAttributes.declaredAttributeId3))

      val response =
        request(data = emptyData, path = s"consumers/${Common.consumerId}/attributes", verb = HttpMethods.GET)

      implicit def attributeJsonFormat: RootJsonFormat[Attribute] = jsonFormat9(Attribute)

      implicit val fromEntityUnmarshallerAttributes: FromEntityUnmarshaller[Attributes] =
        sprayJsonUnmarshaller[Attributes](jsonFormat3(Attributes))

      val body = Await.result(Unmarshal(response.entity).to[Attributes], Duration.Inf)

      body.certified.count(
        _ == AttributeManagementService.toApi(ClientAttributes.certifiedAttribute).futureValue
      ) shouldBe 1

      body.declared.toSet shouldBe Set(
        AttributeManagementService.toApi(ClientAttributes.declaredAttributeId1).futureValue,
        AttributeManagementService.toApi(ClientAttributes.declaredAttributeId2).futureValue,
        AttributeManagementService.toApi(ClientAttributes.declaredAttributeId3).futureValue
      )

      body.verified.toSet shouldBe Set.empty

    }

    "retrieve all attributes owned by a customer (customer without declared attributes)" in {

      (mockJWTReader
        .getClaims(_: String))
        .expects(*)
        .returning(mockSubject(UUID.randomUUID().toString))
        .once()

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
          None,
          Some(Common.consumerId),
          None,
          None,
          Some(PurposeManagementDependency.PurposeState.ACTIVE)
        )
        .returns(Future.successful(Seq(TestDataTwo.purpose, TestDataFour.purpose)))

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataTwo.eserviceId)
        .returns(Future.successful(TestDataTwo.eService))
        .once()

      (mockCatalogManagementService
        .getEServiceById(_: String)(_: UUID))
        .expects(Common.bearerToken, TestDataFour.eserviceId)
        .returns(Future.successful(TestDataFour.eService))
        .once()

      (mockPartyManagementService
        .getPartyAttributes(_: String)(_: UUID))
        .expects(Common.bearerToken, UUID.fromString(Common.consumerId))
        .returns(Future.successful(Seq(Common.certifiedAttribute)))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.certifiedAttribute)
        .returns(Future.successful[ClientAttribute](ClientAttributes.certifiedAttribute))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.verifiedAttributeId1)
        .returns(Future.successful[ClientAttribute](ClientAttributes.verifiedAttributeId1))

      (mockAttributeManagementService
        .getAttribute(_: String)(_: String))
        .expects(Common.bearerToken, Common.verifiedAttributeId3)
        .returns(Future.successful[ClientAttribute](ClientAttributes.verifiedAttributeId3))

      val response =
        request(data = emptyData, path = s"consumers/${Common.consumerId}/attributes", verb = HttpMethods.GET)
      implicit def attributeJsonFormat: RootJsonFormat[Attribute] = jsonFormat9(Attribute)

      implicit val fromEntityUnmarshallerAttributes: FromEntityUnmarshaller[Attributes] =
        sprayJsonUnmarshaller[Attributes](jsonFormat3(Attributes))

      val body = Await.result(Unmarshal(response.entity).to[Attributes], Duration.Inf)

      body.certified.count(
        _ == AttributeManagementService.toApi(ClientAttributes.certifiedAttribute).futureValue
      ) shouldBe 1

      body.declared.toSet shouldBe Set.empty

      body.verified.toSet shouldBe Set(
        AttributeManagementService.toApi(ClientAttributes.verifiedAttributeId1).futureValue,
        AttributeManagementService.toApi(ClientAttributes.verifiedAttributeId3).futureValue
      )

    }

  }

}
