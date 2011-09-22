{-# OPTIONS_GHC -XQuasiQuotes #-}
module Language.Lsl.Internal.BuiltInModules(avEventGen) where

import Language.Lsl.Syntax
import Language.Lsl.QQ(lslm)
import Language.Lsl.Internal.Constants(lslForgeAvatarKey,lslForgeAvatarPos,lslForgeAvatarRot,lslForgeAvatarName)

avEventGenAST = [$lslm|$module
        integer LSLFORGE_AVATAR_KEY = $integer:lslForgeAvatarKey;
        integer LSLFORGE_AVATAR_POS = $integer:lslForgeAvatarPos;
        integer LSLFORGE_AVATAR_ROT = $integer:lslForgeAvatarRot;
        integer LSLFORGE_AVATAR_NAME= $integer:lslForgeAvatarName;
        
        string mkTouch(string primKey, float duration) {
            return "AvatarTouch {avatarTouchPrimKey = LSLKey{unLslKey=\"" + primKey + "\"}, avatarTouchDuration = " + (string) duration + "}";
        }
        string mkFaceTouch(string primKey, float duration, integer face, float s, float t) {
            return "AvatarFaceTouch {avatarTouchPrimKey = LSLKey{unLslKey=\"" + primKey + "\"}, avatarTouchDuration = " + (string) duration + 
                ", avatarTouchFace = " + (string)face + ", avatarTouchST = (" + (string) s + "," + (string) t + ")}";
        }
        string mkWhisper(integer chan, string message) {
            return "AvatarWhisper { avatarChatChannel = " + (string)chan + ", avatarChatMessage = \"" + message + "\"}";
        }
        string mkSay(integer chan, string message) {
            return "AvatarSay { avatarChatChannel = " + (string)chan + ", avatarChatMessage = \"" + message + "\"}";
        }
        string mkShout(integer chan, string message) {
            return "AvatarShout { avatarChatChannel = " + (string)chan + ", avatarChatMessage = \"" + message + "\"}";
        }
        string mkPay(string primKey, integer amount) {
            return "AvatarPay { avatarPayPrimKey = LSLKey{unLslKey=\"" + primKey + "\"}, avatarPayAmount = " + (string) amount + "}";
        }
        string mkControl(integer newControlBits) {
            return "AvatarControl { avatarNewControlBits = " + (string) newControlBits + "}";
        }
        string mkHTTPRequest(string url, string method, string body, string ip, string userAgent) {
            return "AvatarHTTPRequest { avatarHTTPRequestURL = \"" + url + "\", avatarHTTPRequestMethod = \"" +
                method + "\", avatarHTTPRequestBody = \"" + body + "\", avatarHTTPRequestIP = \"" + ip +
                "\", avatarHTTPRequestUserAgent = \"" + userAgent + "\"}";
        }
    |]
    
avEventGen = ("$avEventGen", avEventGenAST)
