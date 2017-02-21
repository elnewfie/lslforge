module Language.Lsl.Internal.Evaluation(
    ScriptInfo(..),
    EvalResult(..),
    Event(..)) where

import Data.Map(Map)
import Language.Lsl.Internal.Key(LSLKey(..))
import Language.Lsl.Internal.Type(LSLValue)
import Language.Lsl.Internal.Breakpoint(Breakpoint)

--type ScriptInfo = (String,Int,String,String) -- (object id, prim index, script name, prim key)
data ScriptInfo a = ScriptInfo { scriptInfoObjectKey :: LSLKey,
                                 scriptInfoPrimIndex :: Int,
                                 scriptInfoScriptName :: String,
                                 scriptInfoPrimKey :: LSLKey,
                                 scriptInfoCurrentEvent :: Maybe (Event a) }
    deriving (Show)

data EvalResult = EvalIncomplete | EvalComplete (Maybe String) | YieldTil Int
                | BrokeAt Breakpoint
    deriving (Show)

data Event a = Event { eventName :: String, eventValues :: [LSLValue a], eventInfo :: Map String (LSLValue a) }
    deriving (Show)
