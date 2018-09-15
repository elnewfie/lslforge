module Language.Lsl.Internal.ExecInfo(emitExecutionInfo) where

import Language.Lsl.Internal.Exec(ExecutionInfo(..),FrameInfo(..))
import Language.Lsl.Syntax(SourceContext(..),TextLocation(..))
import Language.Lsl.Internal.Key(LSLKey(..))
import Language.Lsl.Internal.Type(LSLValue(..))
import Language.Lsl.Internal.XmlCreate(emit,emitSimple)

emitExecutionInfo (ExecutionInfo name line threadInfo) =
    emit "script-state" [] [emitSimple "sourceElement" [] name,
                            emitSimple "currentLine" [] (show line),
                            emitThreadInfo threadInfo]

emitThreadInfo (FrameInfo name frames) =
    emit "threadInfo" [] [emitSimple "name" [] name,
                          emitFrames frames]

emitFrames frames =
    emit "frames" [] (map emitFrame frames)

emitFrame (name,ctx,line,bindings) =
    emit "frame" [] [emitSimple "name" []  name,
                     emitCtx ctx,
                     emitLine line,
                     emit "bindings" [] (map emitBinding bindings)]
    where emitCtx Nothing = id
          emitCtx (Just (SourceContext { srcTextLocation = txtl })) = emitSimple "file" [] ( textName txtl )
          emitLine Nothing = id
          emitLine (Just i) = emitSimple "line" [] (show i)

emitBinding (name,val) =
    emit "binding" [] [emitSimple "name" [] name,
                       emitVal val]

emitVal' t s = emitSimple "val" [("class",t)] s
emitVal (IVal i) = emitVal' "integer-value" (show i)
emitVal (FVal f) = emitVal' "float-value" (show f)
emitVal (SVal s) = emitVal' "string-value" s
emitVal (KVal k) = emitVal' "key-value" (unLslKey k)
emitVal (VVal x y z) =
    emit "val" [("class","vector-value")]
        [emitSimple "x" [] (show x),
         emitSimple "y" [] (show y),
         emitSimple "z" [] (show z)]
emitVal (RVal x y z s) =
    emit "val" [("class","rotation-value")]
        [emitSimple "x" [] (show x),
         emitSimple "y" [] (show y),
         emitSimple "z" [] (show z),
         emitSimple "s" [] (show s)]
emitVal (LVal l) =
    emit "val" [("class","list-value")] [emit "elements" [] (map emitVal l)]
