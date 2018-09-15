module Language.Lsl.Internal.AvEvents(AvatarOutputEvent(..),
                    AvatarInputEvent(..)) where

import Language.Lsl.Internal.Key(LSLKey(..))
import Language.Lsl.Internal.Util(LSLInteger)

data AvatarOutputEvent =
      AvatarTouch { avatarTouchPrimKey :: LSLKey, avatarTouchDuration :: Float }
    | AvatarWhisper { avatarChatChannel :: LSLInteger, avatarChatMessage :: String }
    | AvatarSay { avatarChatChannel :: LSLInteger, avatarChatMessage :: String }
    | AvatarShout { avatarChatChannel :: LSLInteger, avatarChatMessage :: String }
    | AvatarPay { avatarPayPrimKey :: LSLKey, avatarPayAmount :: LSLInteger }
    | AvatarControl { avatarNewControlBits :: LSLInteger }
    | AvatarFaceTouch { avatarTouchPrimKey :: LSLKey,
                        avatarTouchDuration :: Float,
                        avatarTouchFace :: LSLInteger,
                        avatarTouchST :: (Float,Float) }
    | AvatarHTTPRequest { avatarHTTPRequestURL :: String,
                          avatarHTTPRequestMethod :: String,
                          avatarHTTPRequestBody :: String,
                          avatarHTTPRequestIP :: String,
                          avatarHTTPRequestUserAgent :: String }
    deriving (Read,Show)

data AvatarInputEvent =
      AvatarOwnerSay { avatarOwnerSayPrimKey :: LSLKey, avatarOwnerSayMsg :: String }
    | AvatarHearsChat { avatarHearsChatFromName :: String, avatarHearsChatFromKey :: LSLKey, avatarHearsChatMsg :: String }
    | AvatarDialog { avatarDialogMessage :: String, avatarDialogButtons :: [String],
                          avatarDialogChannel :: LSLInteger, avatarDialogSourceObject :: LSLKey }
    | AvatarLoadURL { avatarLoadURLMessage :: String, avatarLoadURLAddress :: String }
    | AvatarMapDestination { avatarMapDestination :: String, avatarMapDestinationPosition :: (Float,Float,Float) }
    | AvatarHTTPResponse { avatarHTTPResponseKey :: LSLKey, avatarHTTPResponseStatus :: LSLInteger,
        avatarHTTPResponseBody :: String }
    | AvatarHTTPRequestKey { avatarHTTPResponseKey :: LSLKey }
    | AvatarHTTPBadRequest
    deriving (Read,Show)
