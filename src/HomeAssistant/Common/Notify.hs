-- | Implements
module HomeAssistant.Common.Notify (
    NotificationData(..),
    Notification,
    notification,
    setNotificationTitle,
    setNotificationData,
    notify
) where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.Text as T

import Deriving.Aeson

import HomeAssistant.Client

--------------------------------------------------------------------------------

-- | The name of the notifications service.
serviceName :: T.Text
serviceName = "notify"

-- | Enumerates different notification platforms that are supported by HA.
-- Different platforms have different capabilities and understand different
-- options in their payloads.
data NotificationPlatform
    -- | Generic.
    = OtherPlatform
    -- | iOS/macOS
    | Apple
    deriving (Eq, Show)

-- |
-- See https://companion.home-assistant.io/docs/notifications/actionable-notifications.
data NotificationAction = MkNotificationAction {
    -- | The text to display on the button.
    notificationActionTitle :: T.Text,
    -- | The key data to send to HA when the button is pressed.
    --
    -- Can be set to the string @"REPLY"@ to prompt for the string to return to HA.
    --
    -- On Android, this must be set to the string @"URI"@ to open the optional URI that can be specified.
    notificationActionAction :: T.Text,
    -- | Optional, the URI to open when the button is pressed.
    notificationActionURI :: Maybe T.Text,
    -- | Set to @"textInput"@ to prompt for text.
    notificationActionBehavior :: Maybe T.Text
} deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "notificationAction") NotificationAction

data AppleNotificationData = MkAppleNotificationData {
    -- | A URI to open when the notification is clicked on,
    -- see https://companion.home-assistant.io/docs/notifications/notifications-basic/#opening-a-url
    appleNotificationUrl :: T.Text,
    -- | A list of actions that should be available as buttons when the notification is pressed.
    appleNotificationAction :: Maybe [NotificationAction]
} deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "appleNotification") AppleNotificationData


-- | Represents platform-specific data for a notification.
data NotificationData (platform :: NotificationPlatform) where
    -- | A general constructor that can be used to pass arbitrary JSON data to
    -- the notification provider.
    OtherNotificationData :: Value -> NotificationData 'OtherPlatform
    AppleNotificationData :: AppleNotificationData -> NotificationData 'Apple

instance Eq (NotificationData platform) where
    (OtherNotificationData l) == (OtherNotificationData r) = l == r
    (AppleNotificationData l) == (AppleNotificationData r) = l == r

instance Show (NotificationData platform) where
    show (OtherNotificationData d) = show d
    show (AppleNotificationData d) = show d

instance FromJSON (NotificationData OtherPlatform) where
    parseJSON = fmap OtherNotificationData . parseJSON

instance FromJSON (NotificationData Apple) where
    parseJSON = fmap AppleNotificationData . parseJSON

instance ToJSON (NotificationData platform) where
    toJSON (OtherNotificationData v) = toJSON v
    toJSON (AppleNotificationData v) = toJSON v

data Notification platform = MkNotification {
    -- | The notification message. This is required.
    notificationMessage :: T.Text,
    -- | The title of the notification. Will default to the Home Assistant app name on the
    -- target device if not set.
    notificationTitle :: Maybe T.Text,
    -- | Platform-specific data associated with the notification.
    --
    -- See the constructors of `NotificationData` for more information and the documentation
    -- at https://www.home-assistant.io/integrations/notify/
    notificationData :: Maybe (NotificationData platform),
    -- | Platform-specific target for the notification.
    notificationTarget :: Maybe Value
} deriving (Generic, Eq, Show)
  deriving (ToJSON) via CustomJSON (JSONOptions "notification") (Notification platform)

instance FromJSON (Notification OtherPlatform) where
    parseJSON = withObject "Notification" $ \v ->
        MkNotification <$> v .: "message" <*> v .: "title" <*> v .: "data" <*> v .: "target"

-- | 'notification' @message@ constructs a `Notification` containing @message@.
-- Can be used to then change other properties of the resulting `Notification`.
notification :: T.Text -> Notification platform
notification message = MkNotification{
    notificationMessage = message,
    notificationTitle = Nothing,
    notificationData = Nothing,
    notificationTarget = Nothing
}

-- | 'setNotificationTitle' @title notification@ sets @notification@'s @title@.
setNotificationTitle ::
    Maybe T.Text ->
    Notification platform ->
    Notification platform
setNotificationTitle title n = n{ notificationTitle = title }

-- | 'setNotificationData' @data notification@ sets @notification@'s @data@.
setNotificationData ::
    Maybe (NotificationData platform) ->
    Notification platform ->
    Notification platform
setNotificationData d n = n{ notificationData = d }

-- | 'notify' @device notification@ is a wrapper around `callService` that uses
-- the notifications service to send @notification@ to @device@.
notify :: T.Text -> Notification platform -> HA Value
notify device = callService serviceName device . Just . toJSON

--------------------------------------------------------------------------------
