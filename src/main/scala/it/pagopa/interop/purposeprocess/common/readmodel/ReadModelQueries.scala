package it.pagopa.interop.purposeprocess.common.readmodel

import it.pagopa.interop.purposemanagement.model.purpose.PersistentPurpose
import it.pagopa.interop.purposemanagement.model.persistence.JsonFormats._
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.PurposeVersionStateConverter
import it.pagopa.interop.purposeprocess.model.PurposeVersionState
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Aggregates.{`match`, count, project, sort}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Projections.{computed, fields, include}
import org.mongodb.scala.model.Sorts.ascending

import scala.concurrent.{ExecutionContext, Future}

object ReadModelQueries {

  def listPurposes(
    name: Option[String],
    eServicesIds: List[String],
    consumersIds: List[String],
    states: List[PurposeVersionState],
    offset: Int,
    limit: Int
  )(readModel: ReadModelService)(implicit ec: ExecutionContext): Future[PaginatedResult[PersistentPurpose]] = {
    val query = listPurposesFilters(name, eServicesIds, consumersIds, states)

    for {
      // Using aggregate to perform case insensitive sorting
      //   N.B.: Required because DocumentDB does not support collation
      eServices <- readModel.aggregate[PersistentPurpose](
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
      count     <- readModel.aggregate[TotalCountResult](
        "purposes",
        Seq(
          `match`(query),
          count("totalCount"),
          project(computed("data", Document("""{ "totalCount" : "$totalCount" }""")))
        ),
        offset = 0,
        limit = Int.MaxValue
      )
    } yield PaginatedResult(results = eServices, totalCount = count.headOption.map(_.totalCount).getOrElse(0))
  }

  def listPurposesFilters(
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
    val eServicesIdsFilter = mapToVarArgs(eServicesIds.map(Filters.eq("data.id", _)))(Filters.or)
    val consumersIdsFilter = mapToVarArgs(consumersIds.map(Filters.eq("data.consumerId", _)))(Filters.or)
    val nameFilter         = name.map(Filters.regex("data.name", _, "i"))

    mapToVarArgs(eServicesIdsFilter.toList ++ consumersIdsFilter.toList ++ statesFilter.toList ++ nameFilter.toList)(
      Filters.and
    ).getOrElse(Filters.empty())
  }

  def mapToVarArgs[A, B](l: Seq[A])(f: Seq[A] => B): Option[B] = Option.when(l.nonEmpty)(f(l))
}
