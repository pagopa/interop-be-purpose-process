package it.pagopa.pdnd.interop.uservice.purposeprocess.common.system

import com.typesafe.config.{Config, ConfigFactory}

import scala.jdk.CollectionConverters._

object ApplicationConfiguration {
  lazy val config: Config = ConfigFactory.load()

  lazy val serverPort: Int          = config.getInt("purpose-process.port")
  lazy val jwtAudience: Set[String] = config.getStringList("purpose-process.jwt.audience").asScala.toSet

  lazy val agreementManagementURL: String = config.getString("purpose-process.services.agreement-management")
  lazy val catalogManagementURL: String   = config.getString("purpose-process.services.catalog-management")
  lazy val partyManagementURL: String     = config.getString("purpose-process.services.party-management")
  lazy val purposeManagementURL: String   = config.getString("purpose-process.services.purpose-management")

  lazy val storageContainer: String = config.getString("purpose-process.storage.container")
  lazy val storagePath: String      = config.getString("purpose-process.storage.risk-analysis-path")

}
