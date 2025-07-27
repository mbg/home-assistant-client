-- | Types used by Home Assistant.
module HomeAssistant.Types (
    JSONOptions,
    UnitSystem(..),
    Config(..),
    Service(..),
    ServiceDomain(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text
import Data.Map.Lazy qualified as M

import Deriving.Aeson

import GHC.TypeLits

--------------------------------------------------------------------------------

-- | The custom JSON encoding/decoding options for HA types.
--
-- HA does not like fields that are set to @null@ - they must be omitted.
type JSONOptions (prefix :: Symbol) =
  '[OmitNothingFields, FieldLabelModifier '[StripPrefix prefix, CamelToSnake]]

--------------------------------------------------------------------------------

-- | Represents units that Home Assistant is configured to use.
data UnitSystem = MkUnitSystem {
    -- | Identifies the unit used for distances.
    unitSystemLength :: Text,
    -- | Identifies the unit used for mass.
    unitSystemMass :: Text,
    -- | Identifies the unit used for temperatures.
    unitSystemTemperature :: Text,
    -- | Identifies the unit used for volumes.
    unitSystemVolume :: Text
} deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "unitSystem") UnitSystem

-- | Represents Home Assistant configurations.
data Config = MkConfig {
    configComponents :: [Text],
    configConfigDir :: Text,
    configLocationName :: Text,
    configTimeZone :: Text,
    configElevation :: Int,
    configLatitude :: Double,
    configLongitude :: Double,
    -- | The units that Home Assistant is configured to use.
    configUnitSystem :: UnitSystem,
    configVersion :: Text,
    configWhitelistExternalDirs :: [Text]
} deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "config") Config

-- | Represents Home Assistant services.
data Service = MkService {
    serviceName :: Text,
    serviceDescription :: Text,
    serviceFields :: M.Map Text Value
} deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "service") Service

-- | Represents Home Assistant service domains.
data ServiceDomain = MkServiceDomain {
    -- | The domain name.
    sdDomain :: Text,
    -- | The services in this domain as a mapping from names to services.
    sdServices :: M.Map Text Service
} deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "sd") ServiceDomain

--------------------------------------------------------------------------------
