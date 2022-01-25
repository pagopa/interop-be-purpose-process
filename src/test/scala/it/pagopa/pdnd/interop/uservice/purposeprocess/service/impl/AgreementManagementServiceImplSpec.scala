package it.pagopa.pdnd.interop.uservice.purposeprocess.service.impl

import akka.actor.ActorSystem
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import it.pagopa.pdnd.interop.uservice.purposemanagement.client.model.{VerifiedAttribute, VerifiedAttributeSeed}
import it.pagopa.pdnd.interop.uservice.purposeprocess.SpecHelper
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.PurposePayload
import it.pagopa.pdnd.interop.uservice.purposeprocess.server.impl.PurposeManagementDependency
import it.pagopa.pdnd.interop.uservice.purposeprocess.service.PurposeManagementService
import it.pagopa.pdnd.interop.uservice.catalogmanagement.client.model._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.concurrent.ExecutionContextExecutor

class PurposeManagementServiceImplSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with Matchers
    with ScalaFutures
    with SpecHelper
    with PurposeManagementDependency {

  implicit val testSystem: ActorSystem                    = system.classicSystem
  implicit val executionContext: ExecutionContextExecutor = system.executionContext

  private val attribute1 = UUID.randomUUID()
  private val attribute2 = UUID.randomUUID()
  private val attribute3 = UUID.randomUUID()
  private val attribute4 = UUID.randomUUID()
  private val attribute5 = UUID.randomUUID()

  "Attribute verification" when {

    "evaluating certified attributes" should {
      "succeed if all single certified attributes are satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1, attribute2).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(single = Some(AttributeValue(attribute2.toString, explicitAttributeVerification = false)))
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.futureValue shouldBe true
      }

      "succeed if at least one grouped certified attribute is satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(group =
                Some(
                  Seq(
                    AttributeValue(attribute1.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute2.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute3.toString, explicitAttributeVerification = false)
                  )
                )
              )
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.futureValue shouldBe true
      }

      "succeed if single and grouped certified attributes are satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1, attribute3).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(group =
                Some(
                  Seq(
                    AttributeValue(attribute2.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute3.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute4.toString, explicitAttributeVerification = false)
                  )
                )
              )
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.futureValue shouldBe true
      }

      "fail if a single certified attribute is not satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1, attribute2).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(single = Some(AttributeValue(attribute3.toString, explicitAttributeVerification = false)))
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if a single certified attribute is not satisfied (no consumer attributes)" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(single = Some(AttributeValue(attribute3.toString, explicitAttributeVerification = false)))
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if all grouped certified attributes are not satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1, attribute2).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(group =
                Some(
                  Seq(
                    AttributeValue(attribute3.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute4.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute5.toString, explicitAttributeVerification = false)
                  )
                )
              )
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if a single certified attribute is satisfied but grouped is not satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1, attribute2).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(group = Some(Seq(AttributeValue(attribute3.toString, explicitAttributeVerification = false))))
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if a grouped certified attribute is satisfied but single is not satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute3).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(group = Some(Seq(AttributeValue(attribute3.toString, explicitAttributeVerification = false))))
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }
    }

    "evaluating verified attributes" should {
      "succeed if all single verified attributes are satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq.empty,
            declared = Seq.empty,
            verified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(single = Some(AttributeValue(attribute2.toString, explicitAttributeVerification = false)))
            )
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(
            VerifiedAttribute(id = attribute1, verified = Some(true)),
            VerifiedAttribute(id = attribute2, verified = Some(true))
          )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.futureValue shouldBe true
      }

      "succeed if at least one grouped verified attribute is satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq.empty,
            declared = Seq.empty,
            verified = Seq(
              Attribute(group =
                Some(
                  Seq(
                    AttributeValue(attribute1.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute2.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute3.toString, explicitAttributeVerification = false)
                  )
                )
              )
            )
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(
            VerifiedAttribute(id = attribute1, verified = Some(true)),
            VerifiedAttribute(id = attribute1, verified = None),
            VerifiedAttribute(id = attribute1, verified = Some(false))
          )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.futureValue shouldBe true
      }

      "succeed if single and grouped verified attributes are satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq.empty,
            declared = Seq.empty,
            verified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(group =
                Some(
                  Seq(
                    AttributeValue(attribute2.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute3.toString, explicitAttributeVerification = false)
                  )
                )
              )
            )
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(
            VerifiedAttribute(id = attribute1, verified = Some(true)),
            VerifiedAttribute(id = attribute2, verified = Some(true)),
            VerifiedAttribute(id = attribute3, verified = None)
          )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.futureValue shouldBe true
      }

      "fail if a single verified attribute has not been verified" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq.empty,
            declared = Seq.empty,
            verified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(single = Some(AttributeValue(attribute2.toString, explicitAttributeVerification = false)))
            )
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(
            VerifiedAttribute(id = attribute1, verified = Some(true)),
            VerifiedAttribute(id = attribute2, verified = None)
          )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if a single verified attribute has been rejected" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq.empty,
            declared = Seq.empty,
            verified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(single = Some(AttributeValue(attribute2.toString, explicitAttributeVerification = false)))
            )
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(
            VerifiedAttribute(id = attribute1, verified = Some(true)),
            VerifiedAttribute(id = attribute2, verified = Some(false))
          )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if all grouped verified attributes have not been verified" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq.empty,
            declared = Seq.empty,
            verified = Seq(
              Attribute(group =
                Some(
                  Seq(
                    AttributeValue(attribute1.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute2.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute3.toString, explicitAttributeVerification = false)
                  )
                )
              )
            )
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq(
          VerifiedAttribute(id = attribute1, verified = None),
          VerifiedAttribute(id = attribute2, verified = None),
          VerifiedAttribute(id = attribute3, verified = None)
        )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if all grouped verified attributes have been rejected" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq.empty,
            declared = Seq.empty,
            verified = Seq(
              Attribute(group =
                Some(
                  Seq(
                    AttributeValue(attribute1.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute2.toString, explicitAttributeVerification = false),
                    AttributeValue(attribute3.toString, explicitAttributeVerification = false)
                  )
                )
              )
            )
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq(
          VerifiedAttribute(id = attribute1, verified = Some(false)),
          VerifiedAttribute(id = attribute2, verified = Some(false)),
          VerifiedAttribute(id = attribute3, verified = Some(false))
        )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if a single verified attribute is satisfied but grouped is not satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(group = Some(Seq(AttributeValue(attribute2.toString, explicitAttributeVerification = false))))
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq(
          VerifiedAttribute(id = attribute1, verified = Some(true)),
          VerifiedAttribute(id = attribute2, verified = Some(false))
        )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if a grouped verified attribute is satisfied but single is not satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified = Seq(
              Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false))),
              Attribute(group = Some(Seq(AttributeValue(attribute2.toString, explicitAttributeVerification = false))))
            ),
            declared = Seq.empty,
            verified = Seq.empty
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq(
          VerifiedAttribute(id = attribute1, verified = Some(false)),
          VerifiedAttribute(id = attribute2, verified = Some(true))
        )

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

    }

    "evaluating both certified and verified attributes" should {
      "succeed if no attribute is required in the EService" in {

        val consumerAttributesIds: Seq[String] =
          Seq(attribute1, attribute2, attribute3).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(certified = Seq.empty, declared = Seq.empty, verified = Seq.empty)
        val purposeVerifiedAttributes: Seq[VerifiedAttribute] = Seq.empty

        val f =
          PurposeManagementService
            .verifyAttributes(consumerAttributesIds, eserviceAttributes, purposeVerifiedAttributes)

        f.futureValue shouldBe true
      }

      "succeed if certified and verified attributes are satisfied" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified =
              Seq(Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false)))),
            declared = Seq.empty,
            verified =
              Seq(Attribute(single = Some(AttributeValue(attribute2.toString, explicitAttributeVerification = false))))
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(VerifiedAttribute(id = attribute2, verified = Some(true)))

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.futureValue shouldBe true
      }

      "fail if certified attributes are satisfied but not verified attributes" in {
        val consumerAttributesIds: Seq[String] = Seq(attribute1).map(_.toString)

        val eserviceAttributes: Attributes =
          Attributes(
            certified =
              Seq(Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false)))),
            declared = Seq.empty,
            verified =
              Seq(Attribute(single = Some(AttributeValue(attribute2.toString, explicitAttributeVerification = false))))
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(VerifiedAttribute(id = attribute2, verified = None))

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

      "fail if verified attributes are satisfied but not certified attributes" in {
        val consumerAttributesIds: Seq[String] = Seq.empty

        val eserviceAttributes: Attributes =
          Attributes(
            certified =
              Seq(Attribute(single = Some(AttributeValue(attribute1.toString, explicitAttributeVerification = false)))),
            declared = Seq.empty,
            verified =
              Seq(Attribute(single = Some(AttributeValue(attribute2.toString, explicitAttributeVerification = false))))
          )

        val purposeVerifiedAttributes: Seq[VerifiedAttribute] =
          Seq(VerifiedAttribute(id = attribute2, verified = Some(true)))

        val f =
          PurposeManagementService.verifyAttributes(
            consumerAttributesIds,
            eserviceAttributes,
            purposeVerifiedAttributes
          )

        f.failed.futureValue shouldBe a[RuntimeException]
      }

    }

  }

  "attributes extraction" should {

    "retrieve all verified attributes owned by a consumer if all attributes are verified as true" in {

      val expected: Set[UUID] = Set(
        UUID.fromString(Common.verifiedAttributeId1),
        UUID.fromString(Common.verifiedAttributeId2),
        UUID.fromString(Common.verifiedAttributeId3)
      )

      val f = PurposeManagementService.extractVerifiedAttribute(purposesAllTrue)

      f.futureValue shouldBe expected
    }

    "retrieve no attributes if the attributes are verified as false" in {

      val expected: Set[UUID] = Set.empty

      val f = PurposeManagementService.extractVerifiedAttribute(purposesAllFalse)

      f.futureValue shouldBe expected
    }

    "retrieve no attributes if each attribute is true/false at the same time" in {

      val expected: Set[UUID] = Set.empty

      val f = PurposeManagementService.extractVerifiedAttribute(purposesSameTrueFalse)

      f.futureValue shouldBe expected
    }

    "retrieve all verified attributes owned by a consumer, excluding attributes set true/false at the same time" in {

      val expected: Set[UUID] =
        Set(UUID.fromString(Common.verifiedAttributeId1), UUID.fromString(Common.verifiedAttributeId3))

      val f = PurposeManagementService.extractVerifiedAttribute(purposesExcludingFalse)

      f.futureValue shouldBe expected
    }
  }

  "certified attributes check" should {

    "pass if attributes match" in {

      val attributeId1 = UUID.randomUUID().toString
      val attributeId2 = UUID.randomUUID().toString
      val attributeId3 = UUID.randomUUID().toString

      val consumerAttributes = Seq(attributeId1, attributeId2, attributeId3)

      val eservice: EService = EService(
        id = UUID.randomUUID(),
        producerId = UUID.randomUUID(),
        name = "name",
        description = "description",
        technology = EServiceTechnology.REST,
        attributes = Attributes(
          certified = Seq(
            Attribute(
              single = Some(AttributeValue(id = attributeId1, explicitAttributeVerification = false)),
              group = None
            ),
            Attribute(
              single = None,
              group = Some(
                Seq(
                  AttributeValue(id = attributeId2, explicitAttributeVerification = false),
                  AttributeValue(id = attributeId3, explicitAttributeVerification = false)
                )
              )
            )
          ),
          declared = Seq.empty[Attribute],
          verified = Seq.empty[Attribute]
        ),
        descriptors = Seq.empty[EServiceDescriptor]
      )

      val expected: EService = eservice

      val f = PurposeManagementService.verifyCertifiedAttributes(consumerAttributes, eservice)

      f.futureValue shouldBe expected

    }

    "not pass if attributes do not match" in {

      val attributeId1 = UUID.randomUUID().toString
      val attributeId2 = UUID.randomUUID().toString
      val attributeId3 = UUID.randomUUID().toString

      val consumerAttributes = Seq(attributeId1, attributeId2, attributeId3)

      val eservice: EService = EService(
        id = UUID.randomUUID(),
        producerId = UUID.randomUUID(),
        name = "name",
        description = "description",
        technology = EServiceTechnology.REST,
        attributes = Attributes(
          certified = Seq(
            Attribute(
              single = Some(AttributeValue(id = UUID.randomUUID().toString, explicitAttributeVerification = false)),
              group = None
            ),
            Attribute(
              single = None,
              group = Some(
                Seq(
                  AttributeValue(id = UUID.randomUUID().toString, explicitAttributeVerification = false),
                  AttributeValue(id = UUID.randomUUID().toString, explicitAttributeVerification = false)
                )
              )
            )
          ),
          declared = Seq.empty[Attribute],
          verified = Seq.empty[Attribute]
        ),
        descriptors = Seq.empty[EServiceDescriptor]
      )

      val f = PurposeManagementService.verifyCertifiedAttributes(consumerAttributes, eservice)

      f.failed.futureValue shouldBe a[RuntimeException]

    }
  }

  "apply implicit verification" should {
    "not work if explicitAttributeVerification is set true" in {

      val expected = Seq(
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId1),
          verified = None,
          validityTimespan = None
        ),
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId2),
          verified = None,
          validityTimespan = None
        ),
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId3),
          verified = None,
          validityTimespan = None
        )
      )

      val f =
        PurposeManagementService.applyImplicitVerification(verifiedAttributesAllSetTrue, customerVerifiedAttributes)

      f.futureValue shouldBe expected
    }

    "works if explicitAttributeVerification is set false" in {

      val expected = Seq(
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId1),
          verified = Some(true),
          validityTimespan = None
        ),
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId2),
          verified = Some(true),
          validityTimespan = None
        ),
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId3),
          verified = None,
          validityTimespan = None
        )
      )

      val f =
        PurposeManagementService.applyImplicitVerification(verifiedAttributesAllSetFalse, customerVerifiedAttributes)

      f.futureValue shouldBe expected
    }

    "works only where the explicit verification is not required" in {

      val expected = Seq(
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId1),
          verified = None,
          validityTimespan = None
        ),
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId2),
          verified = Some(true),
          validityTimespan = None
        ),
        VerifiedAttributeSeed(
          id = UUID.fromString(Common.verifiedAttributeId3),
          verified = None,
          validityTimespan = None
        )
      )

      val f =
        PurposeManagementService.applyImplicitVerification(verifiedAttributesMixed, customerVerifiedAttributes)

      f.futureValue shouldBe expected
    }
  }

  "validate payload" should {
    "work if there are no purposes related to payload information" in {
      val eserviceId   = UUID.randomUUID()
      val descriptorId = UUID.randomUUID()
      val consumerId   = UUID.randomUUID()
      val payload      = PurposePayload(eserviceId = eserviceId, descriptorId = descriptorId, consumerId = consumerId)
      val purposes     = Seq(TestDataOne.purpose, TestDataTwo.purpose)

      val result = PurposeManagementService.validatePayload(payload, purposes)
      result.futureValue shouldBe payload
    }

    "not work if there are purposes related to payload information" in {

      val payload = PurposePayload(
        eserviceId = TestDataOne.eserviceId,
        descriptorId = TestDataOne.descriptorId,
        consumerId = UUID.fromString(Common.consumerId)
      )
      val purposes = Seq(TestDataOne.purpose, TestDataTwo.purpose)

      val result = PurposeManagementService.validatePayload(payload, purposes)
      result.failed.futureValue shouldBe a[RuntimeException]
    }

  }

  "purpose status active check" should {
    "work if purpose is in active status" in {

      val result = PurposeManagementService.isActive(TestDataOne.purpose)
      result.futureValue shouldBe TestDataOne.purpose
    }

    "not work if purpose is not in active status" in {

      val result = PurposeManagementService.isActive(TestDataFive.purpose)
      result.failed.futureValue shouldBe a[RuntimeException]
    }

  }

  "purpose status pending check" should {
    "work if purpose is in pending status" in {

      val result = PurposeManagementService.isPending(TestDataFive.purpose)
      result.futureValue shouldBe TestDataFive.purpose
    }

    "not work if purpose is not in pending status" in {

      val result = PurposeManagementService.isPending(TestDataOne.purpose)
      result.failed.futureValue shouldBe a[RuntimeException]
    }

  }

  "purpose status suspended check" should {
    "work if purpose is in suspended status" in {

      val result = PurposeManagementService.isSuspended(TestDataSix.purpose)
      result.futureValue shouldBe TestDataSix.purpose
    }

    "not work if purpose is not in suspended status" in {

      val result = PurposeManagementService.isSuspended(TestDataOne.purpose)
      result.failed.futureValue shouldBe a[RuntimeException]
    }

  }

}
