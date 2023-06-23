package it.pagopa.interop.purposeprocess

import akka.actor.ActorSystem
import it.pagopa.interop._

import scala.concurrent.ExecutionContextExecutor

package object service {

  type AuthorizationManagementInvoker    = authorizationmanagement.client.invoker.ApiInvoker
  type AuthorizationManagementPurposeApi = authorizationmanagement.client.api.PurposeApi
  type AuthorizationManagementClientApi  = authorizationmanagement.client.api.ClientApi

  type PurposeManagementInvoker = purposemanagement.client.invoker.ApiInvoker
  type PurposeManagementApi     = purposemanagement.client.api.PurposeApi

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
}
