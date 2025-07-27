-- | Client library for the Home Assistant API.
module HomeAssistant.Client (
    API,
    HA,
    JSONOptions,
    StateChange,
    status,
    config,
    services,
    callService,
    mkHomeAssistantEnv,
    module Servant.Client
) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Data
import Data.Text
import Data.ByteString.Char8 qualified as C8

import Deriving.Aeson

import Network.HTTP.Client (Manager, requestHeaders)
import Network.HTTP.Types

import Servant.API
import Servant.Client

import HomeAssistant.Types

--------------------------------------------------------------------------------

-- | The Home Assistant API as a type.
type API = "api" :> Endpoints
type Endpoints
    = Get '[JSON] Value
 :<|> "config" :>
      Get '[JSON] Config
 :<|> "services" :>
      Get '[JSON] [ServiceDomain]
 :<|> "services" :>
      Capture "domain" Text :>
      Capture "service" Text :>
      ReqBody '[JSON] (Maybe Value) :>
      Post '[JSON] Value

api :: Proxy API
api = Proxy

type HA = ClientM

--------------------------------------------------------------------------------

status :: HA Value
config :: HA Config
services :: HA [ServiceDomain]

--------------------------------------------------------------------------------

-- | Represents information about the state of an entity.
data StateChange = MkStateChange {
  stateChangeAttributes :: Value,
  stateChangeEntityID :: Text,
  stateChangeLastChanged :: Text,
  stateChangeState :: Text
} deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "stateChange") StateChange

-- | 'callService' @domain service body@ calls @service@ in @domain@ with an
-- optional @body@. The result depends on the service.
callService :: Text -> Text -> Maybe Value -> HA Value

--------------------------------------------------------------------------------

status
  :<|> config
  :<|> services
  :<|> callService = client api

--------------------------------------------------------------------------------

authorize :: C8.ByteString -> ClientEnv -> ClientEnv
authorize token env = env{ makeClientRequest = mkRequest } where
    mkRequest url req = do
        -- Construct the request using the default function.
        baseReq <- defaultMakeClientRequest url req
        -- Then inject the Authorization header with the HA token.
        pure baseReq{
            requestHeaders =
                (hAuthorization, "Bearer " <> token) : requestHeaders baseReq
        }

mkHomeAssistantEnv :: C8.ByteString -> Manager -> BaseUrl -> ClientEnv
mkHomeAssistantEnv token httpManager address =
    authorize token $ mkClientEnv httpManager address

--------------------------------------------------------------------------------
