{-# OPTIONS_GHC -XFlexibleContexts -XNoMonomorphismRestriction -XTemplateHaskell #-}
module Language.Lsl.Internal.CompilationServer where

--import Control.Monad
--import Control.Monad.Instances
import Control.Monad.Error

--import Data.Data
import Data.Either
import Data.Generics
import Data.Generics.Extras.Schemes
import qualified Data.Map as M

import Language.Lsl.Internal.Compiler
import Language.Lsl.Internal.Load (loadScript)
import Language.Lsl.Internal.Pragmas
import Language.Lsl.Syntax
import Language.Lsl.Parse
import Language.Lsl.Internal.Util
import qualified Language.Lsl.Internal.XmlCreate as E
import Language.Lsl.Internal.SerializationGenerator
import Language.Lsl.Internal.DOMCombinators
import Language.Lsl.Internal.SerializationInstances(
    jrep'Maybe,jrep'Either,jrep'Tuple2,jrep'Tuple3)
import System.Directory
import System.FilePath(replaceExtension)

-- a module or a script
data CodeElement = CodeElement { codeElementName :: String, codeElementText :: String }
                deriving (Show,Eq)

data CompilationCommand = Init (Bool,[(String,String)],[(String,String)]) -- compile all the modules and scripts (writing out .lsl files!)
                        | UpdateModule (String,String) -- update the given module, recompiling affected scripts (and writing out .lsl files!)
                        | UpdateScript (String,String) -- compile the given script (writing out the .lsl file)
                        | RemoveScript (String,String)
                        | RemoveModule (String,String)
                        | CheckModule CodeElement -- check the module
                        | CheckScript CodeElement -- check the script
                deriving (Show,Eq)

data CompilationResponse = FullSourceValidation ([CompilationStatus],[CompilationStatus])
                         | ModuleResponse (LModule,[ErrInfo])
                         | ScriptResponse (LSLScript,[ErrInfo])

data EPKind = EPFunc | EPHandler deriving (Show,Eq)

data EPSummary = EPSummary { epKind :: EPKind, epName :: String, epType :: LSLType, epParams :: [(String,LSLType)] }
    deriving (Show,Eq)
data GlobalSummary = GlobalSummary { globalName :: String, globalType :: LSLType }
     deriving (Show,Eq)

data CompilationStatus = CompilationStatus { csName :: !String, csInfo :: !(Either [ErrInfo] ([GlobalSummary],[EPSummary])) }
    deriving (Show,Eq)

data ErrInfo = ErrInfo (Maybe TextLocation) String deriving (Show,Eq)

toErrInfo (Nothing,msg) = ErrInfo Nothing msg
toErrInfo (Just srcCtx, msg) = ErrInfo (Just $ srcTextLocation srcCtx) msg

parseErrorToErrInfo pe =
    let (x,y) = (sourcePosToTextLocation $ errorPos pe,
            showErrorMessages "or" "unknown parse error"
                "expecting" "unexpected" "end of input" (errorMessages pe))
    in ErrInfo (Just x) y

sourcePosToTextLocation pos = (TextLocation line col line col name)
    where line = sourceLine pos
          col = sourceColumn pos
          name = sourceName pos

gsummary :: Data a => a -> [Either GlobalSummary EPSummary]
gsummary =
    everythingBut (++) (lslQ `extQ` fsum `extQ` gsum)
  where
    gsum (GDecl (Ctx _ (Var n t)) _) =
      ([Left $ GlobalSummary n t], False)
    fsum (FuncDec fnm t parms) =
      ([Right $ EPSummary EPFunc (ctxItem fnm) t (map ((\ (Var n t) -> (n,t)) . ctxItem) parms)], False)

ssummary :: [Ctx State] -> [EPSummary]
ssummary = concatMap go
    where go (Ctx _ (State nm hs)) = map (hsum (ctxItem nm)) hs
          hsum snm (Ctx _ (Handler hnm parms _)) =
              EPSummary EPHandler (snm ++ "." ++ ctxItem hnm) LLVoid (map ((\ (Var n t) -> (n,t)) . ctxItem) parms)

moduleSummary :: LModule -> ([GlobalSummary],[EPSummary])
moduleSummary (LModule gdefs _) = partitionEithers (gsummary gdefs)

scriptSummary :: CompiledLSLScript -> ([GlobalSummary],[EPSummary])
scriptSummary (CompiledLSLScript _ gs fs ss) = (map sumg gs,map sumf fs ++ ssummary ss)
    where sumg (GDecl (Ctx _ (Var n t)) _) = GlobalSummary n t
          sumf (Ctx _ (Func (FuncDec fnm t parms) _)) =
              EPSummary EPFunc (ctxItem fnm) t (map ((\ (Var n t) -> (n,t)) . ctxItem) parms)

validationSummary :: (AugmentedLibrary,[(String,Validity CompiledLSLScript)]) -> ([CompilationStatus],[CompilationStatus])
validationSummary (ms,ss) = (msum,ssum)
    where msum = map mkMSum ms
          ssum = map mkSSum ss
          mkMSum (nm, Left err) = CompilationStatus nm $ Left (map toErrInfo $ codeErrs err)
          mkMSum (nm, Right (lmodule,_)) = CompilationStatus nm $ Right (moduleSummary lmodule)
          mkSSum (nm, Left err) = CompilationStatus nm $ Left (map toErrInfo $ codeErrs err)
          mkSSum (nm, Right cscript) = CompilationStatus nm $ Right (scriptSummary cscript)

data Tup3 a b c = Tup3 a b c

data Tst = Tst (Double,Int,Char)

data Tst1 = Tst1 (Tup3 Double Int Char)

$(deriveJavaRep ''TextLocation)
$(deriveJavaRep ''LSLType)
$(deriveJavaRep ''EPKind)
$(deriveJavaRep ''EPSummary)
$(deriveJavaRep ''GlobalSummary)
$(deriveJavaRep ''CodeElement)
$(deriveJavaRep ''CompilationCommand)
$(deriveJavaRep ''ErrInfo)
$(deriveJavaRep ''CompilationStatus)
$(deriveJavaRep ''State)
$(deriveJavaRep ''GlobDef)
$(deriveJavaRep ''Handler)
$(deriveJavaRep ''Ctx)
$(deriveJavaRep ''Expr)
$(deriveJavaRep ''SourceContext)
$(deriveJavaRep ''Pragma)
$(deriveJavaRep ''LSLScript)
$(deriveJavaRep ''Statement)
$(deriveJavaRep ''Component)
$(deriveJavaRep ''Func)
$(deriveJavaRep ''FuncDec)
$(deriveJavaRep ''Var)
$(deriveJavaRep ''LModule)
$(deriveJavaRep ''CompilationResponse)
$(deriveJavaRep ''Tup3)
$(deriveJavaRep ''Tst)
$(deriveJavaRep ''Tst1)

data CState = CState {
    optimize :: Bool,
    modulePaths :: M.Map String String,
    scriptPaths :: M.Map String String,
    modules :: M.Map String (Validity (LModule,ModuleInfo)),
    scripts :: M.Map String (Validity CompiledLSLScript) }

emptyCState = CState { optimize = False, modulePaths = M.empty, scriptPaths = M.empty, modules = M.empty, scripts = M.empty }

mkCState (opt,mpaths,spaths) (lib,scripts) =
    CState { optimize = opt, modulePaths = M.fromList mpaths, scriptPaths = M.fromList spaths, modules = M.fromList lib, scripts = M.fromList scripts }

toLib = libFromAugLib . M.toList

handler :: CState -> String -> IO (CState, String)
handler s0 input = case parse elemDescriptor input of
   Left s -> return $ (s0, E.emit "error" [] [showString (E.xmlEscape s)] "")
   Right Nothing -> return $ (s0, E.emit "error" [] [showString ("unexpected root element")] "")
   Right (Just command) -> handleCommand s0 command

handleCommand _ (Init srcInfo) = do
    compilationResult <- compileAndEmit srcInfo
    return (mkCState srcInfo compilationResult, xmlSerialize Nothing (FullSourceValidation $ validationSummary compilationResult) "")
handleCommand cs (UpdateScript scriptInfo) = do
    (id,result) <- loadScript (toLib $ modules cs) scriptInfo
    let cs' = cs { scripts = M.insert id result (scripts cs) }
    renderScriptsToFiles (optimize cs') [(id,result)] [scriptInfo]
    return (cs',xmlSerialize Nothing (FullSourceValidation $ validationSummary (M.toList $ modules cs', M.toList $ scripts cs')) "")
handleCommand cs (UpdateModule minfo) = do -- this doesn't quite do what it says it does (yet)
    let srcInfo = (optimize cs, M.toList (M.insert (fst minfo) (snd minfo) (modulePaths cs)), M.toList (scriptPaths cs))
    handleCommand cs (Init srcInfo)
handleCommand cs (RemoveModule minfo) = do
    let srcInfo = (optimize cs, M.toList (M.delete (fst minfo) (modulePaths cs)),M.toList (scriptPaths cs))
    handleCommand cs (Init srcInfo)
handleCommand cs (RemoveScript scriptInfo) = do
    let cs' = cs { scriptPaths = M.delete (fst scriptInfo) (scriptPaths cs), scripts = M.delete (fst scriptInfo) (scripts cs) }
    let summary = FullSourceValidation $ validationSummary (M.toList (modules cs'), M.toList (scripts cs'))
    let filePath = replaceExtension (snd scriptInfo) ".lsl"
    exists <- doesFileExist filePath
    when exists $ removeFile filePath
    return (cs',xmlSerialize Nothing summary "")
handleCommand cs (CheckModule (CodeElement name text)) =
    let (m, errs) = alternateModuleParser name text
        lib1 = compileLibrary
                (M.toList
                    (M.insert name m
                        (M.fromList
                            [ (n,m) | (n,Right m) <- libFromAugLib $ M.toList (modules cs)])))
        errs' = map parseErrorToErrInfo errs ++ case lookup name lib1 of
                    Nothing -> []
                    Just (Right _) -> []
                    Just (Left err) -> map toErrInfo $ codeErrs err
    in return (cs,xmlSerialize Nothing (ModuleResponse (simplifyModule m,errs')) "")
handleCommand cs (CheckScript (CodeElement name text)) =
    let (s, errs) = alternateScriptParser name text
        lib = libFromAugLib $ M.toList (modules cs)
        errs' = map parseErrorToErrInfo errs ++ case compileLSLScript' lib s of
            Left err -> map toErrInfo $ codeErrs err
            _ -> []
    in return (cs,xmlSerialize Nothing (ScriptResponse (simplifyScript s,errs')) "")

-- take the detail out of a script, leaving just the skeleton...
simplifyScript :: LSLScript -> LSLScript
simplifyScript (LSLScript _ gs ss) = LSLScript "" (map simpG gs) (map simpCS ss)
    where simpCS (Ctx sc s) = (Ctx (simpSC sc) (simpS s))
          simpS (State cn chs) = (State (simpCN cn) (map simpCH chs))
          simpCH (Ctx sc h) = (Ctx (simpSC sc) (simpH h))
          simpH (Handler cn _ _) = Handler (simpCN cn) [] []

-- take the detail out of a script, leaving just the skeleton...
simplifyModule :: LModule -> LModule
simplifyModule (LModule gs _) = LModule (map simpG gs) []

simpG (GV cv _) = GV (simpCV cv) Nothing
simpG (GF cf) = GF (simpCF cf)
simpG (GI cn _ _) = GI (simpCN cn) [] ""
simpCV (Ctx sc v) = (Ctx (simpSC sc) v)
simpCF (Ctx sc f) = (Ctx (simpSC sc) (simpF f))
simpCN (Ctx sc n) = (Ctx (simpSC sc) n)
simpSC Nothing = Nothing
simpSC (Just (SourceContext tl _ _ _)) = (Just (SourceContext tl "" "" []))
simpF (Func fd _) = (Func fd [])

compilationServer :: IO ()
compilationServer = processLinesSIOB emptyCState "quit" handler

codeGen loc pkg = do
    setCurrentDirectory loc
    saveReps pkg $ $(collectReps [''CompilationResponse,''CompilationCommand]) pkg
