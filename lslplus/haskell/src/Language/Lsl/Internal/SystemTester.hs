{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.SystemTester where

import Control.Applicative
import Control.Monad.Error(MonadError(..))
import IO
import Language.Lsl.Internal.BreakpointsDeserialize(bps)
import Language.Lsl.Internal.Compiler(compile)
import Language.Lsl.Internal.DOMProcessing(req,tagit,def,getTag,
    choice,val,liste,text,xmlAccept)
import Language.Lsl.Internal.DOMSourceDescriptor(sources)
import Language.Lsl.Internal.ExecInfo(emitExecutionInfo)
import Language.Lsl.Internal.Key(LSLKey(..))
import Language.Lsl.Internal.Log(LogMessage(..),logLevelToName)
import Language.Lsl.Syntax(libFromAugLib)
import Language.Lsl.Internal.Util(unescape,processLinesS)
import Language.Lsl.Sim(SimCommand(..),SimEvent(..),SimEventArg(..),
    SimStatus(..),SimStateInfo(..),simStep)
import Language.Lsl.WorldDef(world,worldXMLAccept)
import Language.Lsl.Internal.XmlCreate(emit,emitList,emitSimple)

commandFromXML xml = either error id $ xmlAccept command xml
    
command = choice cmds >>= maybe
    (getTag >>= \ t -> throwError ("unrecognized command: " ++ t))
    return
    
cmds = [cmd "sim-continue" SimContinue, cmd "sim-step" SimStep,
    cmd "sim-step-over" SimStepOver, cmd "sim-step-out" SimStepOut]
    
cmd s f = tagit s $ f <$> def "breakpoints" [] bps 
    <*> def "events" [] (liste "event" event)
    
event = SimEvent <$> req "name" text <*> def "args" [] (liste "arg" arg) 
    <*> req "delay" val

arg = SimEventArg <$> req "name" text <*> req "value" text

outputToXML (SimInfo _ log state) = 
    (emit "sim-info" [] [emitLog log,emitState state]) ""
outputToXML (SimEnded _ log state) =
    (emit "sim-ended" [] [emitLog log,emitState state]) ""
outputToXML (SimSuspended _ suspendInfo log state) = 
    (emit "sim-suspended" [] [emitExecutionInfo suspendInfo,
                              emitLog log,
                              emitState state]) ""
                                                                               
emitLog log =
    emit "messages" [] $ map emitMessage (log)

emitMessage logMessage =
    emit "message" [] [ 
        emitSimple "time" [] (show $ logMessageTime logMessage),
        emitSimple "level" [] (logLevelToName $ logMessageLevel logMessage),
        emitSimple "source" [] (logMessageSource logMessage),
        emitSimple "text" [] (logMessageText logMessage)]

emitState state =
    emit "state" [] [ emitSimple "time" [] (show $ simStateInfoTime state),
                      emitPrims (simStateInfoPrims state),
                      emitAvatars (simStateInfoAvatars state),
                      emitScripts (simStateInfoScripts state) ]
emitPrims prims = emitList "prims" emitPrim prims
emitAvatars avatars = emitList "avatars" emitAvatar avatars
emitScripts scripts = emitList "scripts" emitScript scripts

emitPrim (LSLKey key,name) = 
    emit "prim" [] [emitSimple "key" [] key, emitSimple "name" [] name]
emitAvatar (LSLKey key,name) = 
    emit "avatar" [] [emitSimple "key" [] key, emitSimple "name" [] name]
emitScript (LSLKey pk,sname) = 
    emit "script" [] [emitSimple "primKey" [] pk, emitSimple "scriptName" [] sname]

testSystem :: IO ()
testSystem = do
    input <- unescape <$> getLine
    case init input of
        Left s -> error s
        Right (src,worldDef) -> do
            (augLib,scripts) <- compile src
            let runStep state s =
                    let command = commandFromXML s
                        (e,state') = simStep state command
                    in (state',outputToXML e)
            processLinesS (Left (worldDef,scripts,libFromAugLib augLib)) "quit" runStep
    where init s = worldXMLAccept s $
                (,) <$> req "source_files" sources <*> req "world-def" world