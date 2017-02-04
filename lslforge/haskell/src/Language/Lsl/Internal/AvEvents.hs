module Language.Lsl.Internal.AvEvents(AvatarOutputEvent(..),
                    AvatarInputEvent(..)) where

import Language.Lsl.Internal.Key(LSLKey(..))

data AvatarOutputEvent =
      AvatarTouch { avatarTouchPrimKey :: LSLKey, avatarTouchDuration :: Float }
    | AvatarWhisper { avatarChatChannel :: Int, avatarChatMessage :: String }
    | AvatarSay { avatarChatChannel :: Int, avatarChatMessage :: String }
    | AvatarShout { avatarChatChannel :: Int, avatarChatMessage :: String }
    | AvatarPay { avatarPayPrimKey :: LSLKey, avatarPayAmount :: Int }
    | AvatarControl { avatarNewControlBits :: Int }
    | AvatarFaceTouch { avatarTouchPrimKey :: LSLKey,
                        avatarTouchDuration :: Float,
                        avatarTouchFace :: Int,
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
                          avatarDialogChannel :: Int, avatarDialogSourceObject :: LSLKey }
    | AvatarLoadURL { avatarLoadURLMessage :: String, avatarLoadURLAddress :: String }
    | AvatarMapDestination { avatarMapDestination :: String, avatarMapDestinationPosition :: (Float,Float,Float) }
    | AvatarHTTPResponse { avatarHTTPResponseKey :: LSLKey, avatarHTTPResponseStatus :: Int,
        avatarHTTPResponseBody :: String }
    | AvatarHTTPRequestKey { avatarHTTPResponseKey :: LSLKey }
    | AvatarHTTPBadRequest
    deriving (Read,Show)
