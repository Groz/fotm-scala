package info.fotm.armory

import com.typesafe.config.ConfigFactory

object Settings {
  val config = ConfigFactory.load()
  config.checkValid(ConfigFactory.defaultReference())

  // non-lazy fields, we want all exceptions at construct time
  val apiKey = config.getString("keys.apiKey")
  val azureStorageAccount = config.getString("keys.azureStorageAccount")
  val azureStorageKey = config.getString("keys.azureStorageKey")
}
