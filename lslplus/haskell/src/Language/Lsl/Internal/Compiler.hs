-- The Lsl "compiler" parses LSL+ 'modules' and 'scripts', and can then:
--   issue a report on all the errors it has found
--   generate LSL scripts for those LSL+ scripts that successfully 'compiled'
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.Compiler(
    compile,
    main0,
    compileEmitSummarize,
    compileAndEmit,
    formatCompilationSummary,
    renderScriptsToFiles) where

import Control.Monad(when)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import IO(Handle,hGetContents,stdin)
import Language.Lsl.Internal.DOMProcessing(tag,xmlAccept)
import Language.Lsl.Internal.DOMSourceDescriptor(sources)
import Language.Lsl.Internal.Load(loadModules,loadScripts)
import Language.Lsl.Render(renderCompiledScript)
import Language.Lsl.Syntax(AugmentedLibrary(..),CompiledLSLScript(..),Ctx(..),
    Func(..),Global(..),GlobDef(..),Handler(..),LModule(..),SourceContext(..),
    State(..),Validity,Var(..),TextLocation(..),funcName,funcParms,funcType,
    libFromAugLib,CodeErrs(..))
import Language.Lsl.Internal.Type(lslTypeString)
import System.Directory(doesFileExist,removeFile)
import System.FilePath(replaceExtension)
import System.Time(calendarTimeToString,getClockTime,toCalendarTime)
import Language.Lsl.Internal.Optimize(optimizeScript,OptimizerOption(..)) 
import Language.Lsl.Internal.XmlCreate hiding (emit)
import qualified Language.Lsl.Internal.XmlCreate as E

emit s = E.emit s []

readCompileEmit :: Handle -> IO ()
readCompileEmit h =
    do sourceInfo@(optimize,_,scriptInfo) <- readSourceList h
       results@(_,compiledScripts) <- compile sourceInfo
       renderScriptsToFiles optimize compiledScripts scriptInfo
       putStr $ formatCompilationSummary results

compileEmitSummarize sourceInfo@(optimize,_,scriptInfo) = do
   results <- compile sourceInfo
   renderScriptsToFiles optimize (snd results) scriptInfo
   return (results,formatCompilationSummary results)

compileAndEmit sourceInfo@(optimize,_,scriptInfo) = do
   results <- compile sourceInfo
   renderScriptsToFiles optimize (snd results) scriptInfo
   return results
   
main0 = readCompileEmit stdin
      
compile :: (Bool,[(String,String)],[(String,String)]) -> IO (AugmentedLibrary,[(String,Validity CompiledLSLScript)])
compile (_,moduleInfo,scriptInfo) =
    do augLib <- loadModules moduleInfo
       scripts <- loadScripts (libFromAugLib augLib) scriptInfo
       return (augLib,scripts)
       
formatCompilationSummary :: (AugmentedLibrary,[(String,Validity CompiledLSLScript)]) -> String
formatCompilationSummary (augLib, scripts) = 
   let emitModules = emit "modules" (map formatModuleCompilationSummary augLib)
       emitScripts = emit "scripts" (map formatScriptCompilationSummary scripts)
   in emit "summary" [emitModules , emitScripts] ""
   
formatScriptCompilationSummary (name,result) =
    emit "item" 
        ([emit "name" [showString name]] ++ 
        case result of
            Left (CodeErrs errs) -> [emit "status" [emit "ok" [showString "false"], emit "errs" (map formatErr errs)]]
            Right (CompiledLSLScript _ globals funcs states) ->
                [emit "status" [emit "ok" [showString "true"]],
                emit "entryPoints" (map emitFunc funcs ++ concatMap stateEntryPointEmitters states),
                emit "globals" (map emitGlobal globals)])
 
formatModuleCompilationSummary (name,result) =
    emit "item"
        ([emit "name" [showString name]] ++
        case result of
            Left (CodeErrs errs) -> [emit "status" [emit "ok" [showString "false"], emit "errs" (map formatErr errs)]]
            Right (LModule globdefs freevars,(globals,_)) ->
                [emit "status" [emit "ok" [showString "true"]],
                emit "entryPoints" (map emitFunc (funcs globdefs)),
                emit "globals" (map emitGlobal globals ++ map emitFreeVar freevars)])
    where funcs globdefs = [ f | GF f <- globdefs]

emitFunc (Ctx _ (Func fd _)) =
    emit "entryPoint" [
        emit "name" [showString (ctxItem $ funcName fd)],
        emit "returnType" [showString (lslTypeString $ funcType fd)],
        emit "params" (emitParams $ map ctxItem (funcParms fd))]

emitGlobal (GDecl (Ctx _ (Var n t)) _) = emit "global" (emitNameTypePair n t)
      
emitFreeVar ctxvar =
   let (Var n t) = ctxItem ctxvar in emit "global" (emitNameTypePair n t)
       
emitNameTypePair n t = [emit "name" [showString n], emit "type" [showString $ lslTypeString t]]
   
stateEntryPointEmitters (Ctx _ (State ctxName handlers)) = map (emitHandler $ ctxItem ctxName) handlers

emitHandler state (Ctx _ (Handler ctxName ctxVars _)) =
    emit "entryPoint" [
        emit "name" [showString (state ++ "." ++ ctxItem ctxName)],
        emit "returnType" [showString "void"],
        emit "params" (emitParams $ map ctxItem ctxVars)]
        
emitParams = map emitParam

emitParam var = emit "param" [emit "name" [showString $ varName var], emit "type" [showString $ lslTypeString $ varType var]]

formatErr (ctx,msg) = 
    emit "itemError" [formatCtx ctx , emit "msg" [showString (xmlEscape msg)]]

formatCtx Nothing = id
formatCtx (Just (SourceContext { srcTextLocation = TextLocation { textLine0 = l0, textColumn0 = c0, textLine1 = l1, textColumn1 = c1, textName = n }})) =
    emit "errLoc" (map (\ (x,y) -> emit x [showString y]) 
                   [("lineStart",show l0),
                   ("columnStart",show c0),
                   ("lineEnd",show l1),
                   ("columnEnd",show c1)])

readSourceList :: Handle -> IO (Bool,[(String,String)],[(String,String)])
readSourceList handle = do
    input <- hGetContents handle
    return $ either error id $ xmlAccept (tag "source_files" >> sources) input

renderScriptsToFiles :: Bool -> [(String,Validity CompiledLSLScript)] -> [(String,String)] -> IO ()
renderScriptsToFiles opt compiledScripts pathTable = 
    let scriptsToRender = 
         [(name, path, script) | (name, Just path,Right script) <- 
             map (\ (name,vs) -> (name, lookup name pathTable,vs)) compiledScripts]
        scriptsToRemove =
         [path | (Just path,Left _) <- map (\ (name,vs) -> (lookup name pathTable,vs)) compiledScripts]
    in do
        clockTime <- getClockTime
        calTime <- toCalendarTime clockTime
        let stamp = calendarTimeToString calTime
        mapM_ (\ (name,path,script) -> 
            renderScriptToFile opt (name ++ " " ++ stamp) path script) scriptsToRender
        mapM_ (removeOutputScript) scriptsToRemove

renderScriptToFile opt stamp path script =
   let newPath = replaceExtension path ".lsl"
       options = if opt then [OptimizationInlining] else []
       text = renderCompiledScript stamp (optimizeScript options script) in B.writeFile newPath (UTF8.fromString text)
       
removeOutputScript path = 
    do exists <- doesFileExist outpath
       when exists $ removeFile outpath
    where outpath = replaceExtension path ".lsl"
           
