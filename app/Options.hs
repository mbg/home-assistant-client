-- | Contains types representing the command-line configuration options of the CLI
-- along with parsers to parse command-line arguments into configuration values.
module Options (
    ConfigCommand(..),
    ServiceCommand(..),
    NotificationCommand(..),
    ClientCommand(..),
    ClientOptions(..),
    parseCmdLineArgs
) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text
import Data.ByteString.Lazy.Char8 qualified as C8

import Options.Applicative

--------------------------------------------------------------------------------

-- | Represents commands related to the Home Assistant configuration.
data ConfigCommand
    = ShowConfig
    | CheckConfig
    | ReloadAllConfig

showConfigP :: Parser ConfigCommand
showConfigP = pure ShowConfig

checkConfigP :: Parser ConfigCommand
checkConfigP = pure ShowConfig

reloadAllConfigP :: Parser ConfigCommand
reloadAllConfigP = pure ReloadAllConfig

--------------------------------------------------------------------------------

-- | Represents commands related to Home Assistant services.
data ServiceCommand
    = ListServices

listServicesP :: Parser ServiceCommand
listServicesP = pure ListServices

--------------------------------------------------------------------------------

-- | Represents commands related to the notification service.
data NotificationCommand
    = SendNotification {
        -- | The name of the service to send the notification to.
        sendNotificationDevice :: Text,
        -- | The message to send.
        sendNotificationMessage :: Text,
        -- | Arbitrary data to send.
        sendNotificationData :: Maybe Value
    }

sendNotificationP :: Parser NotificationCommand
sendNotificationP = SendNotification <$>
    argument str (metavar "DEVICE") <*>
    argument str (metavar "MESSAGE") <*>
    optional (option (eitherReader (eitherDecode . C8.pack)) (long "data"))

--------------------------------------------------------------------------------

data ClientCommand
    = ConfigCommand ConfigCommand
    | ServiceCommand ServiceCommand
    | NotificationCommand NotificationCommand

configP :: Parser ClientCommand
configP = fmap ConfigCommand $ subparser $
    command "show" (info (showConfigP <**> helper) (progDesc "Show the Home Assistant configuration")) <>
    command "check" (info (checkConfigP <**> helper) (progDesc "Check the Home Assistant configuration")) <>
    command "reload-all" (info (reloadAllConfigP <**> helper) (progDesc "Reloads all Home Assistant configurations"))

servicesP :: Parser ClientCommand
servicesP = fmap ServiceCommand $ subparser $
    command "list" (info (listServicesP <**> helper) (progDesc "Lists all Home Assistant services"))

notificationP :: Parser ClientCommand
notificationP = fmap NotificationCommand $ subparser $
    command "send" (info (sendNotificationP <**> helper) (progDesc "Send a notification"))

clientCommandP :: Parser ClientCommand
clientCommandP = subparser $
    command "config" (info (configP <**> helper) (progDesc "Configuration commands")) <>
    command "services" (info (servicesP <**> helper) (progDesc "Services commands")) <>
    command "notification" (info (notificationP <**> helper) (progDesc "Notification service commands"))

--------------------------------------------------------------------------------

data ClientOptions = MkClientOptions {
    clientAddress :: Maybe Text,
    clientOptionsCommand :: ClientCommand
}

optionsP :: Parser ClientOptions
optionsP = MkClientOptions
 <$> optional (strOption (long "address" <> metavar "URL" <> help "The address of the Home Assistant server"))
 <*> clientCommandP

opts :: ParserInfo ClientOptions
opts = info (optionsP <**> helper) idm

parseCmdLineArgs :: IO ClientOptions
parseCmdLineArgs = execParser opts

--------------------------------------------------------------------------------
