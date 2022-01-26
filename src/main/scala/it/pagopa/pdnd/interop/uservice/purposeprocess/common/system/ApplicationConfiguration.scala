package it.pagopa.pdnd.interop.uservice.purposeprocess.common.system

import com.typesafe.config.{Config, ConfigFactory}

object ApplicationConfiguration {
  lazy val config: Config = ConfigFactory.load()

  def serverPort: Int = {
    config.getInt("uservice-purpose-process.port")
  }

  def catalogManagementURL: String = config.getString("services.catalog-management")
  def partyManagementURL: String   = config.getString("services.party-management")
  def purposeManagementURL: String = config.getString("services.purpose-management")

}
