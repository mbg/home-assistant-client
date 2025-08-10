# home-assistant-client: Haskell client for the Home Assistant API

![GitHub](https://img.shields.io/github/license/mbg/home-assistant-client)
[![Haskell](https://github.com/mbg/home-assistant-client/actions/workflows/haskell.yml/badge.svg)](https://github.com/mbg/home-assistant-client/actions/workflows/haskell.yml)
[![Stackage Nightly](https://github.com/mbg/home-assistant-client/actions/workflows/nightly.yml/badge.svg)](https://github.com/mbg/home-assistant-client/actions/workflows/nightly.yml)

A Haskell library for the [Home Assistant API](https://developers.home-assistant.io/docs/api/rest/), allowing you to write Haskell applications which interact with [Home Assistant](https://www.home-assistant.io).

A `ha-client` program that exposes the library functions as CLI commands is also provided.

## Library usage

The Home Assistant API offers fairly general endpoints, for which the `HomeAssistant.Client` module provides client computations. All API endpoints require authorisation with a [long-lived token](https://developers.home-assistant.io/docs/auth_api/#long-lived-access-token). For example, to retrieve a list available services from Home Assistant:

```haskell
import Network.HTTP.Client

defaultAddress :: String
defaultAddress = "http://homeassistant.local:8123/"

main :: IO ()
main = do
    -- `token` should be the long-lived token
    httpManager <- defaultManagerSettings
    address <- parseBaseUrl defaultAddress

    let env = mkHomeAssistantEnv token httpManager address
    result <- runClientM services env
    print result
```

## CLI usage

You must set an environment variable named `HA_TOKEN` to your [long-lived token](https://developers.home-assistant.io/docs/auth_api/#long-lived-access-token). The CLI also inspects the `HA_ADDRESS` environment variable to determine the address of the Home Assistant server. This is optional and will default to `http://homeassistant.local:8123/` if no value is set.

The `ha-client` CLI can be invoked with `--help` to retrieve usage information. All sub-commands also support the `--help` flag to retrieve command-specific help.
