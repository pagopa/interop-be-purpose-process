package it.pagopa.interop.purposeprocess

import akka.actor.ActorSystem
import it.pagopa.interop._
import it.pagopa.interop.purposeprocess.common.system.ApplicationConfiguration
import it.pagopa.interop.selfcare._

import scala.concurrent.ExecutionContextExecutor

package object service {
  type AgreementManagementInvoker = agreementmanagement.client.invoker.ApiInvoker
  type AgreementManagementApi     = agreementmanagement.client.api.AgreementApi

  type AuthorizationManagementInvoker    = authorizationmanagement.client.invoker.ApiInvoker
  type AuthorizationManagementPurposeApi = authorizationmanagement.client.api.PurposeApi
  type AuthorizationManagementClientApi  = authorizationmanagement.client.api.ClientApi

  type PurposeManagementInvoker = purposemanagement.client.invoker.ApiInvoker
  type PurposeManagementApi     = purposemanagement.client.api.PurposeApi

  type CatalogManagementInvoker = catalogmanagement.client.invoker.ApiInvoker
  type CatalogManagementApi     = catalogmanagement.client.api.EServiceApi

  type PartyManagementInvoker = partymanagement.client.invoker.ApiInvoker
  type PartyManagementApi     = partymanagement.client.api.PartyApi

  type PartyManagementApiKeyValue = selfcare.partymanagement.client.invoker.ApiKeyValue

  object PartyManagementApiKeyValue {
    def apply(): PartyManagementApiKeyValue =
      partymanagement.client.invoker.ApiKeyValue(ApplicationConfiguration.partyManagementApiKey)
  }

  object AgreementManagementInvoker {
    def apply(blockingEc: ExecutionContextExecutor)(implicit actorSystem: ActorSystem): AgreementManagementInvoker =
      agreementmanagement.client.invoker.ApiInvoker(agreementmanagement.client.api.EnumsSerializers.all, blockingEc)
  }

  object AgreementManagementApi {
    def apply(baseUrl: String): AgreementManagementApi = agreementmanagement.client.api.AgreementApi(baseUrl)
  }

  object AuthorizationManagementInvoker {
    def apply(blockingEc: ExecutionContextExecutor)(implicit actorSystem: ActorSystem): AuthorizationManagementInvoker =
      authorizationmanagement.client.invoker
        .ApiInvoker(authorizationmanagement.client.api.EnumsSerializers.all, blockingEc)
  }

  object AuthorizationManagementClientApi {
    def apply(baseUrl: String): AuthorizationManagementClientApi =
      authorizationmanagement.client.api.ClientApi(baseUrl)
  }

  object AuthorizationManagementPurposeApi {
    def apply(baseUrl: String): AuthorizationManagementPurposeApi =
      authorizationmanagement.client.api.PurposeApi(baseUrl)
  }

  object PurposeManagementInvoker {
    def apply(blockingEc: ExecutionContextExecutor)(implicit actorSystem: ActorSystem): PurposeManagementInvoker =
      purposemanagement.client.invoker.ApiInvoker(purposemanagement.client.api.EnumsSerializers.all, blockingEc)
  }

  object PurposeManagementApi {
    def apply(baseUrl: String): PurposeManagementApi = purposemanagement.client.api.PurposeApi(baseUrl)
  }

  object CatalogManagementInvoker {
    def apply(blockingEc: ExecutionContextExecutor)(implicit actorSystem: ActorSystem): CatalogManagementInvoker =
      catalogmanagement.client.invoker.ApiInvoker(catalogmanagement.client.api.EnumsSerializers.all, blockingEc)
  }

  object CatalogManagementApi {
    def apply(baseUrl: String): CatalogManagementApi = catalogmanagement.client.api.EServiceApi(baseUrl)
  }

  object PartyManagementInvoker {
    def apply()(implicit actorSystem: ActorSystem): PartyManagementInvoker =
      partymanagement.client.invoker.ApiInvoker(partymanagement.client.api.EnumsSerializers.all)
  }

  object PartyManagementApi {
    def apply(baseUrl: String): PartyManagementApi = partymanagement.client.api.PartyApi(baseUrl)
  }

}
