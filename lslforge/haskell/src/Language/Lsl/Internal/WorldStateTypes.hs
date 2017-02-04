{-# LANGUAGE FlexibleInstances,
             NoMonomorphismRestriction,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             TemplateHaskell
  #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.WorldStateTypes where

import Control.Applicative
import Control.Monad(liftM,ap,MonadPlus(..))
import Control.Monad.State(MonadState(..),StateT(..))
import Control.Monad.Error(ErrorT(..),MonadError(..))
import Data.Map(Map)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Label hiding (get,set)
import Data.LabelExtras
import qualified Data.Set as S

import qualified Language.Lsl.Internal.AvEvents as AvEvent
import Language.Lsl.Internal.Breakpoint(BreakpointManager(..))
import Language.Lsl.Internal.Evaluation(Event(..),ScriptInfo(..),EvalResult(..))
import Language.Lsl.Internal.Key(LSLKey(..))
import Language.Lsl.Internal.Log(LogMessage(..),LogLevel(..))
import Language.Lsl.Syntax(Validity,LModule(..),CompiledLSLScript(..))
import Language.Lsl.Internal.Type(LSLValue(..),LSLType(..))
import Language.Lsl.WorldDef(Prim(..),PrimFace(..),InventoryItem(..),
    InventoryItemIdentification(..),LSLObject(..),Script(..),Avatar(..),
    Region(..),ObjectDynamics(..),Parcel(..),WebHandling(..),PrimType(..),
    Attachment(..),Flexibility(..),LightInfo(..),TextureInfo(..),ScriptId)

import System.Random(StdGen(..))

type WorldEvent = (Int,WorldEventType) -- time, event

type WorldEventQueue = [WorldEvent]

data WorldEventType =
          CreatePrim { wePrimName :: String, wePrimKey :: LSLKey }
        | AddScript (String,String) LSLKey Bool -- script, prim key, activate
        | ResetScript String String -- prim key, script name
        | ResetScripts String -- object name
        | WorldSimEvent {
            worldSimEventName :: String,
            worldSimEventArgs :: [SimEventArg] }
        | DeferredScriptEvent {
            deferredScriptEvent :: Event Float,
            deferredScriptEventTarget :: DeferredScriptEventTarget }
        | Chat {
            chatChannel :: Int,
            chatterName :: String,
            chatterKey :: LSLKey,
            chatMessage :: String,
            chatLocation :: ((Int,Int),(Float,Float,Float)),
            chatRange :: Maybe Float }
        | TimerEvent {
            timerEventInterval :: Float,
            timerAddress :: (LSLKey,String) }
        | PermissionRequestEvent {
            permissionRequestPrim :: LSLKey,
            permissionRequestScript :: String,
            permissionRequestAgent :: LSLKey,
            permissionRequestMask :: Int }
        | SensorEvent {
            sensorAddress :: (LSLKey,String),
            sensorSenseName :: String,
            sensorSenseKey :: LSLKey,
            sensorSenseType :: Int,
            sensorSenseRange :: Float,
            sensorSenseArc :: Float,
            sensorRepeat :: Maybe Float }
        | XMLRequestEvent {
            xmlRequestSource :: XMLRequestSourceType,
            xmlRequestChannel :: LSLKey,
            xmlRequestIData :: Int,
            xmlRequestSData :: String }
        | HTTPRequestEvent {
            httpRequestSource :: (LSLKey,String),
            httpRequestKey :: LSLKey,
            httpRequestURL :: String,
            httpRequestMethod :: String,
            httpRequestMimetype :: String,
            httpRequestBodyMaxlength :: Int,
            httpRequestVerifyCert :: Int,
            httpRequestBody :: String }
        | XMLReplyEvent {
            xmlRequestKey :: LSLKey,
            xmlRequestChannel :: LSLKey,
            xmlRequestMessageId :: LSLKey,
            xmlRequestSData :: String,
            xmlRequestIData :: Int }
        | DialogEvent {
            dialogAgent :: LSLKey,
            dialogMessage :: String,
            dialogButtons :: [String],
            dialogChannel :: Int,
            dialogSourceObject :: LSLKey }
        | RezObjectEvent {
            rezObjectLinkSet :: [Prim],
            rezObjectPos :: (Float,Float,Float),
            rezObjectVel :: (Float,Float,Float),
            rezObjectRot :: (Float,Float,Float,Float),
            rezObjectStartParam :: Int,
            rezObjectRezzer :: LSLKey,
            rezObjectCopy :: Bool,
            rezObjectAtRoot :: Bool }
        | ResetScriptEvent {
            resetScriptPrimKey :: LSLKey,
            resetScriptScriptName :: String }
        | DetachCompleteEvent {
            detachObject :: LSLKey,
            detachAvatar :: LSLKey }
        | GiveAvatarInventoryEvent {
            giveAvatarInventoryKey :: LSLKey,
            giveAvatarInventoryFolder :: String, -- null if none
            giveAvatarInventoryItems :: [InventoryItem] }
        | AvatarOutputEvent {
            avatarOutputEventKey :: LSLKey,
            avatarOutputEventVal :: AvEvent.AvatarOutputEvent }
        | AvatarInputEvent {
            avatarInputEventKey :: LSLKey,
            avatarInputEventVal :: AvEvent.AvatarInputEvent }
    deriving (Show)

data XMLRequestSourceType = XMLRequestInternal { xmlRequestTag :: String }
                          | XMLRequestExternal { xmlRequestTag :: String }
    deriving (Show)

data DeferredScriptEventTarget =
      -- pushes to a specific script in a prim
      DeferredScriptEventScriptTarget (LSLKey,String)
      -- pushes to all scripts in prim
    | DeferredScriptEventPrimTarget LSLKey
      -- pushes to all scripts in all prims in object
    | DeferredScriptEventObjectTarget LSLKey
    deriving (Show)


data Touch = Touch {
    touchAvatarKey :: LSLKey,
    touchPrimKey :: LSLKey,
    touchFace :: Int,
    touchST :: (Float,Float),
    touchStartTick :: Int,
    touchEndTick :: Int  }
    deriving (Show)

data SimEvent = SimEvent { simEventName :: String, simEventArgs :: [SimEventArg], simEventDelay :: Int }
    deriving (Show)
data SimEventArg = SimEventArg { simEventArgName :: String, simEventArgValue :: String }
    deriving (Show)

data Listener = Listener {
    listenerPrimKey :: LSLKey,
    listenerScriptName :: String,
    listenerChannel :: Int,
    listenerName :: String,
    listenerKey :: LSLKey,
    listenerMsg :: String }
    deriving (Show)

type Predef m = ScriptInfo Float -> [LSLValue Float] -> WorldE m (EvalResult,LSLValue Float)
data PredefFunc m = PredefFunc { predefFuncName :: String,
                                 predefFuncResultType :: LSLType,
                                 predef :: Predef m }
     deriving (Show)

instance Monad m => Show (Predef m) where
    showsPrec _ _ = showString "(function :: ScriptInfo -> [LSLValue] -> WorldE m (EvalResult,LSLValue))"

-- an ErrorT/StateT/m Monad stack for the World.  The type is parameterized by
-- the innermost monad...
newtype WorldE m a = WorldE { unWorldE :: ErrorT String ((StateT (World m) m)) a }
    deriving (Monad,MonadPlus)

instance Monad m => MonadState (World m) (WorldE m) where
   get = WorldE { unWorldE = get }
   put v = WorldE { unWorldE = put v }

instance Monad m => MonadError String (WorldE m) where
    throwError e = WorldE { unWorldE = throwError e }
    catchError v f = WorldE { unWorldE = catchError (unWorldE v) (unWorldE . f) }

instance Monad m => Functor (WorldE m) where
    fmap = liftM

instance Monad m => Applicative (WorldE m) where
   pure  = return
   (<*>) = ap

instance Monad m => Alternative (WorldE m) where
   empty = mzero
   (<|>) = mplus

data PendingHTTPResponse = PendingHTTPResponse {
        phrRequesterKey :: LSLKey,
        phrResponderScript :: ScriptId,
        phrBaseURL :: String,
        phrPath :: String,
        phrQuery :: String,
        phrRemoteIP :: String,
        phrUserAgent :: String,
        phrExpire :: Int
    } deriving (Show)

-- a data type that defines the state of the 'world'
data World m = World {
        _sliceSize :: !Int,
        _maxTick :: !Int,
        _nextPause :: !Int,
        _wqueue :: !WorldEventQueue,
        _wlisteners :: !(IM.IntMap (Listener,Bool)),
        _nextListenerId :: !Int,
        _wobjects :: !(Map LSLKey LSLObject),
        _wprims :: !(Map LSLKey Prim),
        _worldScripts :: !(Map (LSLKey,String) Script),
        _inventory :: ![(String,LSLObject)],
        _tick :: !Int,
        _msglog :: ![LogMessage],
        _predefs :: !(Map String (PredefFunc m)),
        _randGen :: !StdGen,
        _wlibrary :: ![(String,Validity LModule)],
        _wscripts :: ![(String,Validity CompiledLSLScript)],
        _worldEventHandler :: !(Maybe (String, [(String,LSLValue Float)])),
        _worldAvatars :: !(Map LSLKey Avatar),
        _worldBreakpointManager :: !BreakpointManager,
        _worldSuspended :: !(Maybe (LSLKey,String)), -- prim-key, script-name, image
        _worldRegions :: !(Map (Int,Int) Region),
        _worldZeroTime :: !Int,
        _worldKeyIndex :: !Integer,
        _worldWebHandling :: !WebHandling,
        _worldOutputQueue :: ![SimEvent],
        _worldPendingHTTPRequests :: ![LSLKey],
        _worldOpenDataChannels :: !(Map LSLKey (LSLKey,String),Map (LSLKey,String) LSLKey),
        _worldXMLRequestRegistry :: !(Map LSLKey XMLRequestSourceType),
        _worldPhysicsTime :: !Int,
        _worldTargetCheckTime :: !Int,
        _worldLastPositions :: !(Map LSLKey (Bool,(Float,Float,Float))),
        _worldCollisions :: !(S.Set (LSLKey,LSLKey)),
        _worldLandCollisions :: !(S.Set LSLKey),
        _worldTouches :: !(Map LSLKey [Touch]),
        _worldTouchCheckTime :: !Int,
        _worldURLRegistry :: !(Map String (LSLKey,ScriptId)),
        _worldPendingHTTPResponses :: !(Map LSLKey PendingHTTPResponse)
    } deriving (Show)

$(mkLabelsAlt [''World,''LSLObject,''ObjectDynamics,''Prim, ''PrimType,
    ''Avatar,''Script,''Attachment,''PrimFace,''Flexibility,''LightInfo,
    ''TextureInfo])
