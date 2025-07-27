-- | Implements
module HomeAssistant.Common.HomeAssistant (
    check,
    reloadAll
) where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.Text as T

import HomeAssistant.Client ( HA, callService )
import HomeAssistant.Types ( Config )

--------------------------------------------------------------------------------

-- | The name of the Home Assistant service.
serviceName :: T.Text
serviceName = "homeassistant"

--------------------------------------------------------------------------------

-- | 'notify' @device notification@ is a wrapper around `callService` that uses
-- the notifications service to send @notification@ to @device@.
check :: HA (Result Config)
check = fromJSON <$> callService serviceName "check_config" Nothing

reloadAll :: HA Value
reloadAll = callService serviceName "reload_all" Nothing

--------------------------------------------------------------------------------
