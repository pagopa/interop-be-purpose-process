package it.pagopa.pdnd.interop.uservice.purposeprocess.common.system

import com.typesafe.config.{Config, ConfigFactory}

object ApplicationConfiguration {
  lazy val config: Config = ConfigFactory.load()

  lazy val serverPort: Int = config.getInt("uservice-purpose-process.port")

  lazy val catalogManagementURL: String = config.getString("services.catalog-management")
  lazy val partyManagementURL: String   = config.getString("services.party-management")
  lazy val purposeManagementURL: String = config.getString("services.purpose-management")

}
