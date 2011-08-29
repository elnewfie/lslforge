module Language.Lsl.Internal.SimMetaData where

import Control.Monad.Identity(Identity(..))
import qualified Data.Map as M

import Language.Lsl.Internal.Type(lslTypeString)
import Language.Lsl.Sim(SimInputEventDefinition(..),SimParam(..),SimParamType(..),eventDescriptors)
import Language.Lsl.Internal.XmlCreate(emit,emitList,emitSimple)

printSimMeta = putStr buildSimMetaData

buildSimMetaData =
    emit "sim-meta-data" [] [
        emitList "eventDescriptors" emitEventDescriptor (M.toList (eventDescriptors :: M.Map String (SimInputEventDefinition Identity)))
    ] ""
    
emitEventDescriptor (_,def) =
    emit "event-def" [] [emitSimple "name" [] (simInputEventName def),
                         emitSimple "description" [] (simInputEventDescription def),
                         emitList "params" emitParam (simInputEventParameters def)]

emitParam p = 
    emit "param" [] [emitSimple "name" [] (simParamName p),
                     emitSimple "description" [] (simParamDescription p),
                     emitParamType (simParamType p)]

emitParamType SimParamPrim = emit "type" [("class","prim")] []
emitParamType SimParamAvatar = emit "type" [("class","avatar")] []
emitParamType (SimParamLSLValue t) = emit "type" [("class","value")] [emitSimple "valueType" [] (lslTypeString t)]
emitParamType (SimParamRootPrim) = emit "type" [("class","object")] []
emitParamType (SimParamKey) = emit "type" [("class","any-key")] []
emitParamType (SimParamScript) = emit "type" [("class","script")] []
