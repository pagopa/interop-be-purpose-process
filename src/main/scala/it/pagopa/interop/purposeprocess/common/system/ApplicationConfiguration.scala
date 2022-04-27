package it.pagopa.interop.purposeprocess.common.system

import com.typesafe.config.{Config, ConfigFactory}

object ApplicationConfiguration {
  val config: Config = ConfigFactory.load()

  val serverPort: Int          = config.getInt("purpose-process.port")
  val jwtAudience: Set[String] = config.getString("purpose-process.jwt.audience").split(",").toSet.filter(_.nonEmpty)

  val agreementManagementURL: String     = config.getString("purpose-process.services.agreement-management")
  val authorizationManagementURL: String = config.getString("purpose-process.services.authorization-management")
  val catalogManagementURL: String       = config.getString("purpose-process.services.catalog-management")
  val partyManagementURL: String         = config.getString("purpose-process.services.party-management")
  val purposeManagementURL: String       = config.getString("purpose-process.services.purpose-management")

  val storageContainer: String = config.getString("purpose-process.storage.container")
  val storagePath: String      = config.getString("purpose-process.storage.risk-analysis-path")

  require(jwtAudience.nonEmpty, "Audience cannot be empty")
}
