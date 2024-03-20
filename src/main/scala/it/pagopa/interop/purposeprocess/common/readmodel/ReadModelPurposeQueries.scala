package it.pagopa.interop.purposeprocess.common.readmodel

import it.pagopa.interop.catalogmanagement.model.CatalogItem
import it.pagopa.interop.catalogmanagement.model.persistence.JsonFormats.ciFormat
import it.pagopa.interop.commons.cqrs.service.ReadModelService
import it.pagopa.interop.purposemanagement.model.persistence.JsonFormats._
import it.pagopa.interop.purposemanagement.model.purpose._
import org.mongodb.scala.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Aggregates._
import org.mongodb.scala.model.Projections.{computed, fields, include}
import org.mongodb.scala.model.Sorts.ascending
import org.mongodb.scala.model.{Field, Filters, Projections}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

object ReadModelPurposeQueries extends ReadModelQuery {

  def getPurpose(
    purposeId: UUID
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Option[PersistentPurpose]] =
    readModel.findOne[PersistentPurpose]("purposes", Filters.eq("data.id", purposeId.toString))

  private def getPurposesFilters(
    eServiceId: Option[UUID],
    consumerId: Option[UUID],
    states: Seq[PersistentPurposeVersionState]
  ): Bson = {
    val eserviceIdFilter = eServiceId.map(id => Filters.eq("data.eserviceId", id.toString))
    val consumerIdFilter = consumerId.map(id => Filters.eq("data.consumerId", id.toString))
    val statesFilter     = mapToVarArgs(
      states
        .map(_.toString)
        .map(Filters.eq("data.versions.state", _))
    )(Filters.or)

    mapToVarArgs(eserviceIdFilter.toList ++ consumerIdFilter.toList ++ statesFilter.toList)(Filters.and)
      .getOrElse(Filters.empty())
  }

  def getPurposes(
    eServiceId: Option[UUID],
    consumerId: Option[UUID],
    states: Seq[PersistentPurposeVersionState],
    offset: Int,
    limit: Int
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Seq[PersistentPurpose]] = {
    val filters: Bson = getPurposesFilters(eServiceId = eServiceId, consumerId = consumerId, states = states)
    readModel.find[PersistentPurpose]("purposes", filters, offset, limit)
  }

  def listPurposes(
    requesterId: UUID,
    title: Option[String],
    eServicesIds: List[String],
    consumersIds: List[String],
    producersIds: List[String],
    states: List[PersistentPurposeVersionState],
    excludeDraft: Boolean,
    offset: Int,
    limit: Int,
    exactMatchOnTitle: Boolean = false
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[PaginatedResult[PersistentPurpose]] = {

    val simpleFilters: Bson = listPurposesFilters(title, eServicesIds, consumersIds, states, exactMatchOnTitle)

    for {
      producersFilter <- producersEservicesFilter(producersIds)
      query = Seq(
        `match`(simpleFilters),
        `match`(listPurposesAuthorizationFilters(excludeDraft)),
        `match`(producersFilter)
      )
      // Using aggregate to perform case insensitive sorting
      //   N.B.: Required because DocumentDB does not support collation
      purposes  <- readModel.aggregate[PersistentPurpose](
        "purposes",
        query ++
          Seq(
            addFields(Field("lowerName", Document("""{ "$toLower" : "$data.title" }"""))),
            sort(ascending("lowerName")),
            project(fields(include("data")))
          ),
        offset = offset,
        limit = limit,
        allowDiskUse = true
      )
      eservices <- purposesEservices(purposes, limit)

      // If requester is not Consumer or Producer, override the risk analysis form with empty value
      authorizedPurposes = purposes.map(p =>
        Option
          .when(p.consumerId == requesterId)(())
          .orElse(
            eservices
              .find(pe => pe.id == p.eserviceId)
              .filter(_.producerId == requesterId)
              .map(_ => ())
          )
          .fold(p.copy(riskAnalysisForm = None))(_ => p)
      )

      // Note: This could be obtained using $facet function (avoiding to execute the query twice),
      //   but it is not supported by DocumentDB
      count <- readModel.aggregate[TotalCountResult](
        "purposes",
        query ++ Seq(count("totalCount"), project(computed("data", Document("""{ "totalCount" : "$totalCount" }""")))),
        offset = 0,
        limit = Int.MaxValue
      )
    } yield PaginatedResult(results = authorizedPurposes, totalCount = count.headOption.map(_.totalCount).getOrElse(0))
  }

  private def listPurposesFilters(
    title: Option[String],
    eServicesIds: List[String],
    consumersIds: List[String],
    states: List[PersistentPurposeVersionState],
    exactMatchOnTitle: Boolean
  ): Bson = {
    // Takes purposes that contain only version with state Archived
    // (purposes that contain version with state == Archived but not versions with state != Archived)
    val archivedStatePartialFilter = states
      .filter(_ == Archived)
      .map(_.toString)
      .distinct
      .map(v =>
        Filters.and(
          Filters.elemMatch("data.versions", Filters.eq("state", v)),
          Filters.not(Filters.elemMatch("data.versions", Filters.ne("state", v)))
        )
      )

    val statesPartialFilter = states
      .filterNot(_ == Archived)
      .map(_.toString)
      .map(Filters.eq("data.versions.state", _))

    val statesFilter       = mapToVarArgs(statesPartialFilter ++ archivedStatePartialFilter)(Filters.or)
    val eServicesIdsFilter = mapToVarArgs(eServicesIds.map(Filters.eq("data.eserviceId", _)))(Filters.or)
    val consumersIdsFilter = mapToVarArgs(consumersIds.map(Filters.eq("data.consumerId", _)))(Filters.or)
    val titleFilter        =
      if (exactMatchOnTitle) title.map(n => Filters.regex("data.title", s"^${escape(n)}$$", "i"))
      else title.map(n => Filters.regex("data.title", escape(n), "i"))
    mapToVarArgs(
      eServicesIdsFilter.toList ++ consumersIdsFilter.toList ++ statesFilter.toList ++ titleFilter.toList // :+ permissionFilter
    )(Filters.and).getOrElse(Filters.empty())
  }

  private def listPurposesAuthorizationFilters(excludeDraft: Boolean): Bson = {
    // Exclude draft purposes only if requested
    // Note: the filter works on the assumption that if a version in Draft exists, it is the only version in the Purpose

    val versionsFilter: Bson =
      if (excludeDraft) Filters.and(Filters.ne("data.versions.state", Draft.toString)) else Filters.empty()

    mapToVarArgs(versionsFilter :: Nil)(Filters.and).getOrElse(Filters.empty())
  }

  private def producersEservicesFilter(
    producersIds: Seq[String]
  )(implicit ec: ExecutionContext, readModel: ReadModelService): Future[Bson] = {

    val producersEservicesIds: Future[Seq[ReadModelId]] =
      mapToVarArgs(producersIds.map(Filters.eq("data.producerId", _)))(Filters.or)
        .fold[Future[Seq[ReadModelId]]](Future.successful(Seq.empty))(filter =>
          readModel
            .find[ReadModelId]("eservices", filter, Projections.include("data.id"), offset = 0, limit = Int.MaxValue)
        )

    producersEservicesIds.map(ids =>
      mapToVarArgs(ids.map(id => Filters.eq("data.eserviceId", id.id.toString)))(Filters.or).getOrElse(Filters.empty())
    )
  }

  private def purposesEservices(purposes: Seq[PersistentPurpose], limit: Int)(implicit
    ec: ExecutionContext,
    readModel: ReadModelService
  ): Future[Seq[CatalogItem]] =
    mapToVarArgs(purposes.map(p => Filters.eq("data.id", p.eserviceId.toString)))(Filters.or)
      .fold[Future[Seq[CatalogItem]]](Future.successful(Seq.empty))(filter =>
        readModel.find[CatalogItem]("eservices", filter, offset = 0, limit = limit)
      )

}
