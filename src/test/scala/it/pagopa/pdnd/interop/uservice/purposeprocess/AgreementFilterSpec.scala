package it.pagopa.pdnd.interop.uservice.purposeprocess

import it.pagopa.pdnd.interop.uservice.purposeprocess.api.impl.PurposeFilter
import it.pagopa.pdnd.interop.uservice.purposeprocess.model.{Purpose, PurposeState, EService, Organization}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class PurposeFilterSpec extends AnyWordSpecLike with Matchers with ScalaFutures {

  private def getTestData: Seq[Purpose] = {
    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    val uuid3 = UUID.randomUUID()
    val uuid4 = UUID.randomUUID()

    val producerOrg1 = Organization("a1", "a1")
    val producerOrg2 = Organization("b1", "b1")
    val producerOrg3 = Organization("c1", "c1")
    val producerOrg4 = Organization("d1", "d1")

    val consumerOrg1 = Organization("cons1", "cons1")
    val consumerOrg2 = Organization("cons2", "cons2")
    val consumerOrg3 = Organization("cons3", "cons3")
    val consumerOrg4 = Organization("cons4", "cons4")

    val eservice1 = EService(UUID.randomUUID(), "eservice1", "1", None)
    val eservice2 = EService(UUID.randomUUID(), "eservice2", "2", None)
    val eservice3 = EService(UUID.randomUUID(), "eservice3", "3", None)
    val eservice4 = EService(UUID.randomUUID(), "eservice4", "4", None)

    Seq(
      Purpose(
        id = uuid1,
        producer = producerOrg1,
        consumer = consumerOrg1,
        eservice = eservice1,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid2,
        producer = producerOrg2,
        consumer = consumerOrg2,
        eservice = eservice2,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid3,
        producer = producerOrg3,
        consumer = consumerOrg3,
        eservice = eservice3,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid4,
        producer = producerOrg4,
        consumer = consumerOrg4,
        eservice = eservice4,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      )
    )
  }

  "an Purpose filter" should {
    "return the same sequence when no latest filter is applied" in {
      //given
      val purposes = getTestData

      //when
      val filtered = PurposeFilter.filterPurposesByLatestVersion(None, purposes)

      //then
      val expectedId = purposes.map(_.id)
      filtered.futureValue.map(_.id) should contain only (expectedId: _*)
    }

    "return the same sequence when FALSE latest filter is applied" in {
      //given
      val purposes = getTestData

      //when
      val filtered = PurposeFilter.filterPurposesByLatestVersion(Some(false), purposes)

      //then
      val expectedId = purposes.map(_.id)
      filtered.futureValue.map(_.id) should contain only (expectedId: _*)
    }
  }

  "return only the latest version for each purpose when latest filter is TRUE" in {
    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    val uuid3 = UUID.randomUUID()
    val uuid4 = UUID.randomUUID()

    val producerOrg1 = Organization("a1", "a1")
    val producerOrg2 = Organization("b1", "b1")
    val producerOrg3 = Organization("c1", "c1")
    val producerOrg4 = Organization("d1", "d1")

    val consumerOrg1 = Organization("cons1", "cons1")
    val consumerOrg2 = Organization("cons2", "cons2")
    val consumerOrg3 = Organization("cons3", "cons3")
    val consumerOrg4 = Organization("cons4", "cons4")

    val eserviceId1 = UUID.randomUUID()
    val eserviceId2 = UUID.randomUUID()
    val eservice1   = EService(eserviceId1, "eservice1", "100", None)
    val eservice2   = EService(eserviceId1, "eservice2", "2", None)
    val eservice3   = EService(eserviceId2, "eservice2", "30", None)
    val eservice4   = EService(eserviceId2, "eservice2", "4", None)

    val purposesToFilter = Seq(
      Purpose(
        id = uuid1,
        producer = producerOrg1,
        consumer = consumerOrg1,
        eservice = eservice1,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid2,
        producer = producerOrg2,
        consumer = consumerOrg2,
        eservice = eservice2,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid3,
        producer = producerOrg3,
        consumer = consumerOrg3,
        eservice = eservice3,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid4,
        producer = producerOrg4,
        consumer = consumerOrg4,
        eservice = eservice4,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      )
    )

    //when
    val filtered = PurposeFilter.filterPurposesByLatestVersion(Some(true), purposesToFilter)

    //then
    filtered.futureValue.map(_.id) should contain only (uuid1, uuid3)
  }

  "return only the latest version for each purpose when latest filter is TRUE and some purpose has wrong version values" in {
    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    val uuid3 = UUID.randomUUID()
    val uuid4 = UUID.randomUUID()

    val producerOrg1 = Organization("a1", "a1")
    val producerOrg2 = Organization("b1", "b1")
    val producerOrg3 = Organization("c1", "c1")
    val producerOrg4 = Organization("d1", "d1")

    val consumerOrg1 = Organization("cons1", "cons1")
    val consumerOrg2 = Organization("cons2", "cons2")
    val consumerOrg3 = Organization("cons3", "cons3")
    val consumerOrg4 = Organization("cons4", "cons4")

    val eserviceId1 = UUID.randomUUID()
    val eserviceId2 = UUID.randomUUID()
    val eservice1   = EService(eserviceId1, "eservice1", "100", None)
    val eservice2   = EService(eserviceId1, "eservice2", "2", None)
    val eservice3   = EService(eserviceId2, "eservice2", "pippo", None)
    val eservice4   = EService(eserviceId2, "eservice2", "4", None)

    val purposesToFilter = Seq(
      Purpose(
        id = uuid1,
        producer = producerOrg1,
        consumer = consumerOrg1,
        eservice = eservice1,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid2,
        producer = producerOrg2,
        consumer = consumerOrg2,
        eservice = eservice2,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid3,
        producer = producerOrg3,
        consumer = consumerOrg3,
        eservice = eservice3,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      ),
      Purpose(
        id = uuid4,
        producer = producerOrg4,
        consumer = consumerOrg4,
        eservice = eservice4,
        state = PurposeState.ACTIVE,
        attributes = Seq.empty,
        suspendedByConsumer = None,
        suspendedByProducer = None,
        eserviceDescriptorId = UUID.randomUUID()
      )
    )

    //when
    val filtered = PurposeFilter.filterPurposesByLatestVersion(Some(true), purposesToFilter)

    //then
    filtered.futureValue.map(_.id) should contain only (uuid1, uuid4)
  }
}
