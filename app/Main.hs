module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe

import System.Environment
import System.Exit

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HomeAssistant.Client
import HomeAssistant.Common.HomeAssistant
import HomeAssistant.Common.Notify

import Options

--------------------------------------------------------------------------------

-- | This is the default address that Home Assistant installations can be found at.
-- Used only if no address is explicitly specified.
defaultAddress :: String
defaultAddress = "http://homeassistant.local:8123/"

--------------------------------------------------------------------------------

managerForBaseUrl :: BaseUrl -> IO Manager
managerForBaseUrl address = newManager $ case baseUrlScheme address of
    Http -> defaultManagerSettings
    Https -> tlsManagerSettings

--------------------------------------------------------------------------------

runAndFormat :: JSON.ToJSON v => ClientEnv -> HA v -> IO ()
runAndFormat env m = runClientM m env >>= \case
    Left err -> print err
    Right v -> LBS.putStr (JSON.encode v)

runConfigCmd :: ClientEnv -> ConfigCommand -> IO ()
runConfigCmd env ShowConfig = runAndFormat env config
runConfigCmd env CheckConfig = runAndFormat env $ do
    result <- check
    case result of
        JSON.Error err -> liftIO $ do
            print err
            exitFailure
        JSON.Success v -> pure v
runConfigCmd env ReloadAllConfig = runAndFormat env reloadAll

runServiceCmd :: ClientEnv -> ServiceCommand -> IO ()
runServiceCmd env ListServices = runAndFormat env services

runNotificationCmd :: ClientEnv -> NotificationCommand -> IO ()
runNotificationCmd env SendNotification{..} = runAndFormat env $ do
    let notificationData = OtherNotificationData <$> sendNotificationData
        payload = setNotificationData notificationData $ notification sendNotificationMessage
    notify sendNotificationDevice payload

-- | 'main' is the main entry point for this application.
main :: IO ()
main = do
    -- Parse command-line arguments
    opts <- parseCmdLineArgs

    mToken <- fmap C8.pack <$> lookupEnv "HA_TOKEN"
    address <- fromMaybe defaultAddress <$> lookupEnv "HA_ADDRESS" >>= parseBaseUrl

    case mToken of
        Nothing -> do
            putStrLn "Error: HA_TOKEN must be set"
            exitFailure
        Just token -> do
            httpManager <- managerForBaseUrl address

            let env = mkHomeAssistantEnv token httpManager address

            case clientOptionsCommand opts of
                ConfigCommand cmd -> runConfigCmd env cmd
                ServiceCommand cmd -> runServiceCmd env cmd
                NotificationCommand cmd -> runNotificationCmd env cmd

--------------------------------------------------------------------------------
