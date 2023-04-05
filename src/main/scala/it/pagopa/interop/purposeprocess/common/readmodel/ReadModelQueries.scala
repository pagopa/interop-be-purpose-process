package it.pagopa.interop.purposeprocess.common.readmodel

import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.purposemanagement.model.persistence.JsonFormats._
import it.pagopa.interop.purposemanagement.model.purpose.{Draft, PersistentPurpose}
import it.pagopa.interop.purposeprocess.api.converters.purposemanagement.PurposeVersionStateConverter
import it.pagopa.interop.purposeprocess.model.PurposeVersionState
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Aggregates._
import org.mongodb.scala.model.{Field, Filters}
import org.mongodb.scala.model.Projections.{computed, fields, include}
import org.mongodb.scala.model.Sorts.ascending

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

object ReadModelQueries {

  def listPurposes(
    requesterId: UUID,
    name: Option[String],
    eServicesIds: List[String],
    consumersIds: List[String],
    producersIds: List[String],
    states: List[PurposeVersionState],
    offset: Int,
    limit: Int
  )(readModel: ReadModelService)(implicit ec: ExecutionContext): Future[PaginatedResult[PersistentPurpose]] = {
    val simpleFilters: Bson = listPurposesFilters(name, eServicesIds, consumersIds, states)
    val query: Seq[Bson]    = Seq(
      `match`(simpleFilters),
      lookup("eservices", "data.eserviceId", "data.id", "eservices"),
      addFields(Field("eservice", Document("""{ $arrayElemAt: [ "$eservices", 0 ] }"""))),
      `match`(Filters.and(listPurposesAuthorizationFilters(requesterId), listPurposesProducersFilters(producersIds)))
    )

    for {
      // Using aggregate to perform case insensitive sorting
      //   N.B.: Required because DocumentDB does not support collation
      purposes <- readModel.aggregate[PersistentPurpose](
        "purposes",
        query ++
          Seq(
            addFields(Field("lowerName", Document("""{ "$toLower" : "$data.title" }"""))),
            sort(ascending("lowerName")),
            // If requester is not Consumer or Producer, override the risk analysis form with null value
            addFields(
              Field(
                "data.riskAnalysisForm",
                Document(s"""{
                            |  $$cond: {
                            |    if: {
                            |      $$or: [
                            |        { $$eq: [ "$$data.consumerId", "${requesterId.toString}" ] },
                            |        { $$eq: [ "$$eservice.data.producerId", "${requesterId.toString}" ] }
                            |      ],
                            |    },
                            |    then: "$$data.riskAnalysisForm",
                            |    else: null,
                            |  },
                            |}""".stripMargin)
              )
            ),
            project(fields(include("data")))
          ),
        offset = offset,
        limit = limit
      )
      // Note: This could be obtained using $facet function (avoiding to execute the query twice),
      //   but it is not supported by DocumentDB
      count    <- readModel.aggregate[TotalCountResult](
        "purposes",
        query ++ Seq(count("totalCount"), project(computed("data", Document("""{ "totalCount" : "$totalCount" }""")))),
        offset = 0,
        limit = Int.MaxValue
      )
    } yield PaginatedResult(results = purposes, totalCount = count.headOption.map(_.totalCount).getOrElse(0))
  }

  private def listPurposesFilters(
    name: Option[String],
    eServicesIds: List[String],
    consumersIds: List[String],
    states: List[PurposeVersionState]
  ): Bson = {
    // Takes purposes that contain only version with state Archived
    // (purposes that contain version with state == Archived but not versions with state != Archived)
    val archivedStatePartialFilter = states
      .filter(_ == PurposeVersionState.ARCHIVED)
      .map(PurposeVersionStateConverter.apiToPersistent)
      .map(_.toString)
      .distinct
      .map(v =>
        Filters.and(
          Filters.elemMatch("data.versions", Filters.eq("state", v)),
          Filters.not(Filters.elemMatch("data.versions", Filters.ne("state", v)))
        )
      )

    val statesPartialFilter = states
      .filterNot(_ == PurposeVersionState.ARCHIVED)
      .map(PurposeVersionStateConverter.apiToPersistent)
      .map(_.toString)
      .map(Filters.eq("data.versions.state", _))

    val statesFilter       = mapToVarArgs(statesPartialFilter ++ archivedStatePartialFilter)(Filters.or)
    val eServicesIdsFilter = mapToVarArgs(eServicesIds.map(Filters.eq("data.eserviceId", _)))(Filters.or)
    val consumersIdsFilter = mapToVarArgs(consumersIds.map(Filters.eq("data.consumerId", _)))(Filters.or)
    val nameFilter         = name.map(Filters.regex("data.title", _, "i"))

    mapToVarArgs(
      eServicesIdsFilter.toList ++ consumersIdsFilter.toList ++ statesFilter.toList ++ nameFilter.toList // :+ permissionFilter
    )(Filters.and).getOrElse(Filters.empty())
  }

  private def listPurposesAuthorizationFilters(requesterId: UUID): Bson = {
    // Include draft purposes only if the requester is the consumer
    // Note: the filter works on the assumption that if a version in Draft exists, it is the only version in the Purpose
    val versionsFilter = Filters.or(
      Filters
        .and(Filters.eq("data.consumerId", requesterId.toString), Filters.ne("data.producerId", requesterId.toString), Filters.eq("data.versions.state", Draft.toString)),
      Filters.ne("data.versions.state", Draft.toString)
    )

    mapToVarArgs(versionsFilter :: Nil)(Filters.and).getOrElse(Filters.empty())
  }

  private def listPurposesProducersFilters(producersIds: Seq[String]): Bson =
    mapToVarArgs(producersIds.map(Filters.eq("eservices.data.producerId", _)))(Filters.or).getOrElse(Filters.empty())

  private def mapToVarArgs[A, B](l: Seq[A])(f: Seq[A] => B): Option[B] = Option.when(l.nonEmpty)(f(l))
}
