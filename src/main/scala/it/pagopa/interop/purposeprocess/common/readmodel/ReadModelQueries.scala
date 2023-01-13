package it.pagopa.interop.purposeprocess.common.readmodel

import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.commons.utils.SprayCommonFormats.uuidFormat
import it.pagopa.interop.purposemanagement.model.persistence.JsonFormats._
import it.pagopa.interop.purposemanagement.model.purpose.{Draft, PersistentPurpose}
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.PurposeVersionStateConverter
import it.pagopa.interop.purposeprocess.model.PurposeVersionState
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Aggregates.{`match`, count, project, sort}
import org.mongodb.scala.model.{Filters, Projections}
import org.mongodb.scala.model.Projections.{computed, fields, include}
import org.mongodb.scala.model.Sorts.ascending
import spray.json.DefaultJsonProtocol.jsonFormat1
import spray.json.RootJsonFormat

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

object ReadModelQueries {

  final case class EServiceId(id: UUID)
  object EServiceId {
    implicit val eIdFormat: RootJsonFormat[EServiceId] = jsonFormat1(EServiceId.apply)
  }

  def listPurposes(
    requesterId: UUID,
    name: Option[String],
    eServicesIds: List[String],
    consumersIds: List[String],
    producersIds: List[String],
    states: List[PurposeVersionState],
    offset: Int,
    limit: Int
  )(readModel: ReadModelService)(implicit ec: ExecutionContext): Future[PaginatedResult[PersistentPurpose]] =
    for {
      // Note: This could be done with a lookup, but we'll keep this simple knowing the db migration is imminent
      producersEServicesIds <- readModel.find[EServiceId](
        "eservices",
        listProducerEServicesIdsFilter(producersIds),
        Projections.include("data.id"),
        offset = 0,
        limit = Int.MaxValue
      )
      query = listPurposesFilters(
        requesterId,
        name,
        eServicesIdsFilter(eServicesIds, producersEServicesIds.map(_.id.toString).toList),
        consumersIds,
        states
      )
      // Using aggregate to perform case insensitive sorting
      //   N.B.: Required because DocumentDB does not support collation
      purposes <- readModel.aggregate[PersistentPurpose](
        "purposes",
        Seq(
          `match`(query),
          project(fields(include("data"), computed("lowerName", Document("""{ "$toLower" : "$data.name" }""")))),
          sort(ascending("lowerName"))
        ),
        offset = offset,
        limit = limit
      )
      // Note: This could be obtained using $facet function (avoiding to execute the query twice),
      //   but it is not supported by DocumentDB
      count    <- readModel.aggregate[TotalCountResult](
        "purposes",
        Seq(
          `match`(query),
          count("totalCount"),
          project(computed("data", Document("""{ "totalCount" : "$totalCount" }""")))
        ),
        offset = 0,
        limit = Int.MaxValue
      )
    } yield PaginatedResult(results = purposes, totalCount = count.headOption.map(_.totalCount).getOrElse(0))

  def listProducerEServicesIdsFilter(producersIds: List[String]): Bson =
    mapToVarArgs(producersIds.map(Filters.eq("data.producerId", _)))(Filters.or).getOrElse(Filters.empty())

  def listPurposesFilters(
    requesterId: UUID,
    name: Option[String],
    eServicesIds: List[String],
    consumersIds: List[String],
    states: List[PurposeVersionState]
  ): Bson = {
    val statesPartialFilter = states
      .map(PurposeVersionStateConverter.apiToPersistent)
      .map(_.toString)
      .map(Filters.eq("data.versions.state", _))

    val statesFilter       = mapToVarArgs(statesPartialFilter)(Filters.or)
    val eServicesIdsFilter = mapToVarArgs(eServicesIds.map(Filters.eq("data.eserviceId", _)))(Filters.or)
    val consumersIdsFilter = mapToVarArgs(consumersIds.map(Filters.eq("data.consumerId", _)))(Filters.or)
    val nameFilter         = name.map(Filters.regex("data.title", _, "i"))

    // Include draft purposes only if the requester is the consumer
    // Note: the filter works on the assumption that if a version in Draft exists, it is the only version in the Purpose
    val permissionFilter = Filters.or(
      Filters
        .and(Filters.eq("data.consumerId", requesterId.toString), Filters.eq("data.versions.state", Draft.toString)),
      Filters.ne("data.versions.state", Draft.toString)
    )

    mapToVarArgs(
      eServicesIdsFilter.toList ++ consumersIdsFilter.toList ++ statesFilter.toList ++ nameFilter.toList :+ permissionFilter
    )(Filters.and).getOrElse(Filters.empty())
  }

  def eServicesIdsFilter(eServicesIds: List[String], producerEServicesIds: List[String]): List[String] =
    (eServicesIds, producerEServicesIds) match {
      case (Nil, seq)   => seq
      case (seq, Nil)   => seq
      case (seq1, seq2) => seq1.intersect(seq2)
    }

  def mapToVarArgs[A, B](l: Seq[A])(f: Seq[A] => B): Option[B] = Option.when(l.nonEmpty)(f(l))
}
