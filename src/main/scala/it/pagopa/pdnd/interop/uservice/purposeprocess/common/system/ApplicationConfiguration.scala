package it.pagopa.pdnd.interop.uservice.purposeprocess.common.system

import com.typesafe.config.{Config, ConfigFactory}

import scala.jdk.CollectionConverters._

object ApplicationConfiguration {
  lazy val config: Config = ConfigFactory.load()

  lazy val serverPort: Int     = config.getInt("uservice-purpose-process.port")
  def jwtAudience: Set[String] = config.getStringList("uservice-purpose-process.jwt.audience").asScala.toSet

  lazy val agreementManagementURL: String = config.getString("services.agreement-management")
  lazy val catalogManagementURL: String   = config.getString("services.catalog-management")
  lazy val partyManagementURL: String     = config.getString("services.party-management")
  lazy val purposeManagementURL: String   = config.getString("services.purpose-management")

  lazy val storageContainer: String = config.getString("uservice-purpose-process.storage.container")
  lazy val storagePath: String      = config.getString("uservice-purpose-process.storage.risk-analysis-path")

}
