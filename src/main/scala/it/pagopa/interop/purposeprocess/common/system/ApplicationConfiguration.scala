package it.pagopa.interop.purposeprocess.common.system

import com.typesafe.config.{Config, ConfigFactory}
import it.pagopa.interop.commons.cqrs.model.ReadModelConfig

object ApplicationConfiguration {
  val config: Config = ConfigFactory.load()

  val serverPort: Int          = config.getInt("purpose-process.port")
  val jwtAudience: Set[String] = config.getString("purpose-process.jwt.audience").split(",").toSet.filter(_.nonEmpty)

  val attributeRegistryManagementURL: String =
    config.getString("purpose-process.services.attribute-registry-management")
  val agreementManagementURL: String         = config.getString("purpose-process.services.agreement-management")
  val authorizationManagementURL: String     = config.getString("purpose-process.services.authorization-management")
  val catalogManagementURL: String           = config.getString("purpose-process.services.catalog-management")
  val purposeManagementURL: String           = config.getString("purpose-process.services.purpose-management")
  val tenantManagementURL: String            = config.getString("purpose-process.services.tenant-management")

  val storageKind: String      = config.getString("purpose-process.storage.kind")
  val storageContainer: String = config.getString("purpose-process.storage.container")
  val storagePath: String      = config.getString("purpose-process.storage.risk-analysis-path")

  val readModelConfig: ReadModelConfig = {
    val connectionString: String = config.getString("purpose-process.read-model.db.connection-string")
    val dbName: String           = config.getString("purpose-process.read-model.db.name")

    ReadModelConfig(connectionString, dbName)
  }
  require(jwtAudience.nonEmpty, "Audience cannot be empty")

}
