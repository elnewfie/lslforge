{-# OPTIONS_GHC -XDeriveDataTypeable -XTypeSynonymInstances -XFlexibleContexts -XGeneralizedNewtypeDeriving
                -XTemplateHaskell -XNoMonomorphismRestriction -XFlexibleInstances #-}
-- | Defines the abstract syntax tree for LSL (and LSL Plus extensions).
module Language.Lsl.Syntax (
    -- Types
    Expr(..),
    Var(..),
    FuncDec (..),
    Func(..),
    LModule(..),
    Component(..),
    Statement(..),
    LSLType(..),
    Handler(..),
    State(..),
    GlobDef(..),
    LSLScript(..),
    Validity,
    Global(..),
    TextLocation(..),
    SourceContext(..),
    Ctx(..),
    CompiledLSLScript(..),
    Library,
    AugmentedLibrary,
    CodeErr,
    CodeErrs(..),
    CtxVar,
    CtxName,
    CtxExpr,
    CtxStmt,
    ModuleInfo,
    Codebase(..),
    -- Values
    fromMCtx,
    ctxItems,
    nullCtx,
    ctxVr2Vr,
    findFunc,
    moduleFromScript,
    findState,
    predefFuncs,
    findFuncDec,
    goodHandlers,
    libFromAugLib,
    isTextLocation,
    isCastValid,
    compileLSLScript,
    compileLSLScript',
    compileLibrary,
    VState,
    emptyValidationState,
    rewriteCtxExpr,
    rmCtx,
    lslQ) where

import Language.Lsl.Internal.Type(Component(..),LSLType(..),lslTypeString)
import Language.Lsl.Internal.Constants(isConstant,findConstType)
import Language.Lsl.Internal.EventSigs(simpleLslEventDescriptors)
import Language.Lsl.Internal.FuncSigs(funcSigs)
import Language.Lsl.Internal.AccessGenerator(genAccessorsForType,genMAccessorsForType)
import Language.Lsl.Internal.Pragmas(Pragma(..))
import Data.Generics
import Data.Data(Data,Typeable)
import Data.List(find,sort,sortBy,nub,nubBy,deleteFirstsBy)
import qualified Data.Map as M
import Data.Maybe(isJust,isNothing)
import Language.Lsl.Internal.Util(LSLInteger,ctx,findM,lookupM,filtMap)
import Control.Monad(when,MonadPlus(..))
import Control.Monad.Except(MonadError(..))
-- import Control.Monad.Error.Class(Error(..))
import qualified Control.Monad.State as S(State)
import Control.Monad.State hiding(State)

-- import Debug.Trace
--trace1 s v = trace (s ++ show v) v

data TextLocation = TextLocation { textLine0 :: Int, textColumn0 :: Int, textLine1 :: Int, textColumn1 :: Int, textName :: String }
    deriving (Show,Eq,Typeable,Data)
data SourceContext = SourceContext { srcTextLocation :: TextLocation, srcPreText :: String, srcPostTxt :: String, srcPragmas :: [Pragma] }
                     deriving (Show,Typeable,Data)

isTextLocation (Just (TextLocation _ _ _ _ _)) = True
isTextLocation _ = False

rmCtx :: Data a => a -> a
rmCtx = everywhere (mkT doNullify)
    where doNullify :: Maybe SourceContext -> Maybe SourceContext
          doNullify _ = Nothing

-- | A wrapper that can associate a source code context with a value (e.g. a syntax value).
data Ctx a = Ctx { srcCtx :: Maybe SourceContext, ctxItem :: a } deriving (Show,Typeable,Data)
instance Functor Ctx where
    fmap f (Ctx c v) = (Ctx c $ f v)

ctxItems = map ctxItem

fromMCtx :: Maybe (Ctx a) -> Maybe a
fromMCtx = fmap ctxItem

nullCtx :: a -> Ctx a
nullCtx = Ctx Nothing

funcNames = map (ctxItem.funcName)

ctxVr2Vr :: (Ctx a,b) -> (a,b)
ctxVr2Vr (Ctx _ name,c) = (name,c)

type CtxVar = Ctx Var

-- | An LSL variable (a name, and a type).
data Var = Var { varName :: String, varType :: LSLType } deriving (Show,Typeable,Data)

type CtxName = Ctx String

-- | An LSL function declaration (the function name and type information, without associated
-- statements.
data FuncDec = FuncDec { funcName :: CtxName, funcType :: LSLType, funcParms :: [CtxVar] }
    deriving (Show,Typeable,Data)

type CtxStmt = Ctx Statement

type CtxFunc = Ctx Func

-- | An LSL function definition (return type, parameters and statements.
data Func = Func FuncDec [CtxStmt] deriving (Show,Typeable,Data)

-- | An LSL Plus module, which is a separately 'compiled' unit that can contain
-- both global variables and functions, but not states or handlers.
data LModule = LModule [GlobDef] [CtxVar]
    deriving (Show,Typeable,Data)

type CtxExpr = Ctx Expr
-- | An LSL expression.
data Expr = IntLit LSLInteger
          | FloatLit Double
          | StringLit String
          | ListExpr [CtxExpr]
          | VecExpr CtxExpr CtxExpr CtxExpr
          | RotExpr CtxExpr CtxExpr CtxExpr CtxExpr
          | KeyLit String
          | Call CtxName [CtxExpr]
          | Add CtxExpr CtxExpr
          | Sub CtxExpr CtxExpr
          | Mul CtxExpr CtxExpr
          | Div CtxExpr CtxExpr
          | Mod CtxExpr CtxExpr
          | Get (CtxName,Component)
          | Set (CtxName,Component) CtxExpr
          | BAnd CtxExpr CtxExpr
          | BOr CtxExpr CtxExpr
          | Xor CtxExpr CtxExpr
          | ShiftL CtxExpr CtxExpr
          | ShiftR CtxExpr CtxExpr
          | And CtxExpr CtxExpr
          | Or CtxExpr CtxExpr
          | Equal CtxExpr CtxExpr
          | NotEqual CtxExpr CtxExpr
          | Lt CtxExpr CtxExpr
          | Le CtxExpr CtxExpr
          | Gt CtxExpr CtxExpr
          | Ge CtxExpr CtxExpr
          | IncBy (CtxName,Component) CtxExpr
          | DecBy (CtxName,Component) CtxExpr
          | MulBy (CtxName,Component) CtxExpr
          | DivBy (CtxName,Component) CtxExpr
          | ModBy (CtxName,Component) CtxExpr
          | PostInc (CtxName,Component)
          | PostDec (CtxName,Component)
          | PreInc (CtxName,Component)
          | PreDec (CtxName,Component)
          | Not CtxExpr
          | Neg CtxExpr
          | Inv CtxExpr
          | Cast LSLType CtxExpr
          | AQString String
          | AQInteger String
          | AQKey String
          | AQFloat String
            deriving (Show,Typeable,Data)

-- | An LSL statement.
data Statement = Compound [CtxStmt]
               | While CtxExpr CtxStmt
               | DoWhile CtxStmt CtxExpr
               | For ([CtxExpr]) (Maybe CtxExpr) ([CtxExpr]) CtxStmt
               | If CtxExpr CtxStmt CtxStmt
               | Decl Var (Maybe CtxExpr)
               | NullStmt
               | Return (Maybe CtxExpr)
               | StateChange String
               | Do CtxExpr
               | Label String
               | Jump String
    deriving (Show,Typeable,Data)

isLabel (Label _) = True
isLabel _ = False

-- | An LSL global variable (this is actually not a source level/syntactic entity -- the set of globals
-- for a script is derived after analyzing all included modules.
data Global = GDecl (Ctx Var) (Maybe Expr)
    deriving (Show,Typeable,Data)

-- | A global definition (a function, a variable, or a module import statement).
data GlobDef = GV CtxVar (Maybe CtxExpr) | GF (Ctx Func) | GI CtxName [(String,String)] String
    deriving (Show,Typeable,Data)
-- | An LSL event handler definition.
data Handler = Handler { handlerName :: CtxName, handlerParams :: [CtxVar], handlerStatements :: [CtxStmt] }
    deriving (Show,Typeable,Data)

-- | The set of valid handlers supported by LSL.
goodHandlers :: [(String,[LSLType])]
goodHandlers = simpleLslEventDescriptors

-- | An LSL state definition.
data State = State CtxName [Ctx Handler]
    deriving (Show,Typeable,Data)

-- | An LSL script.
data LSLScript = LSLScript String [GlobDef] [Ctx State] deriving (Show,Typeable,Data)

type ModuleInfo = ([Global],[Ctx Func])
-- | A collection of modules.
type Library = [(String,Validity LModule)]
-- | A collection of mouldes, augmented with additional derived information.
type AugmentedLibrary = [(String,Validity (LModule,ModuleInfo))]

data Codebase = Codebase { codebaseLib :: AugmentedLibrary, codebaseScripts :: [(String,Validity CompiledLSLScript)] }

lslFunc (name,t,ts) =
    FuncDec (nullCtx name) t (zipWith (\ x y -> nullCtx $ Var [y] x) ts ['a'..])
predefFuncs = map lslFunc funcSigs

findVar name = find (\(Var n _ ) -> n == name)
findType name =
    let f Nothing = Nothing
        f (Just (Var _ t )) = Just t in f . (findVar name)
findFuncDec name = ctx ("finding function " ++ name) . findM (\ fd -> ctxItem (funcName fd) == name)
findState name = ctx ("finding state " ++ name) . findM (\ (State n _) -> ctxItem n == name)
findFunc name = ctx ("finding function " ++ name) . findM (\ (Func fd _) -> ctxItem (funcName fd) == name)

lookupModule :: String -> Library -> Validity LModule
lookupModule name lib =
    case lookup name lib of
        Nothing -> throwError $ CodeErrs [(Nothing, "unknown module")]
        Just (Left (CodeErrs ((_,s):_))) -> throwError $ CodeErrs [(Nothing, "invalid library (" ++ s ++ ")")]
        Just (Right m) -> return m

-- A description of an error and where to find it in the source.
type CodeErr = (Maybe SourceContext,String)
newtype CodeErrs = CodeErrs { codeErrs :: [CodeErr] } deriving (Show)

ctxFromCodeErr = fst
msgFromCodeErr = snd

-- | An error monad for representing validation errors with respect to LSL code.
type Validity a = Either CodeErrs a

--------------------

throwStrError :: String -> Validity a
throwStrError s = throwError $ CodeErrs [(Nothing,s)]

matchTypes LLFloat LLInteger = True
matchTypes dest src = dest == src || (all (`elem` [LLKey,LLString]) [dest,src])

data CompiledLSLScript = CompiledLSLScript {
    scriptComment :: !String,
    scriptGlobals :: ![Global],
    scriptFuncs :: ![Ctx Func],
    scriptStates :: ![Ctx State]}
    deriving (Show)

data RefPos = RefPos { refPosName :: !String, refPosLine :: !Int, refPosCol :: !Int }
    deriving (Show,Eq,Ord)

data ValidationState = ValidationState {
        vsLib :: !Library,
        vsGlobalRegistry :: !(M.Map String (Maybe SourceContext)),
        vsLocalRegistry :: ![M.Map String (Maybe SourceContext)],
        vsLocalVars :: ![[Var]],
        vsRefs :: !(M.Map RefPos SourceContext),
        vsLabels :: ![[String]],
        vsModules :: ![String],
        vsStates :: ![Ctx State],
        vsGlobals :: ![Global],
        vsFuncs :: ![Ctx Func],
        vsErr :: !CodeErrs,
        vsWarn :: !CodeErrs,
        vsNamesUsed :: [String],
        vsGVs :: ![Var],
        vsGFs :: ![FuncDec],
        vsStateNames :: ![String],
        vsEntryPointInfo :: !(LSLType,Bool),
        vsBranchReturns :: !Bool,
        vsHandlersUsed :: ![String],
        vsImports :: ![(String,[(String,String)],String)],
        vsContext :: [Maybe SourceContext]
    }

emptyValidationState = ValidationState {
    vsLib = [],
    vsGlobalRegistry = M.empty,
    vsLocalRegistry = [],
    vsLocalVars = [],
    vsRefs = M.empty,
    vsLabels = [],
    vsModules = [],
    vsStates = [],
    vsGlobals = [],
    vsFuncs = [],
    vsErr = CodeErrs [],
    vsWarn = CodeErrs [],
    vsNamesUsed = [],
    vsGVs = [],
    vsGFs = [],
    vsStateNames = [],
    vsEntryPointInfo = (LLVoid,False),
    vsBranchReturns = False,
    vsHandlersUsed = [],
    vsImports = [],
    vsContext = []
    }
-- generate set'xxx methods for all the record selectors in the above type...
-- warning: all types mentioned in the above declaration must be defined (lexically) prior to
-- the following splice
-- double warning: the 'set' functions generated can only be used in code that
-- lexically follows this splice
$(genAccessorsForType ''ValidationState) -- a Template Haskell splice...
-- this splice requires the above splice, and, because the author of the splice was too lazy to
-- generate type signatures, also requires -XNoMonomorphismRestriction
$(genMAccessorsForType ''ValidationState) -- a Template Haskell splice...

type VState a = S.State ValidationState a

--vsmRegisterGlobal :: Ctx Var -> VState ()
vsmRegisterGlobalName name ctx = get'vsGlobalRegistry >>= put'vsGlobalRegistry . (M.insert name ctx)
vsmRegisterGlobal (Ctx ctx (Var name t)) = vsmRegisterGlobalName name ctx
vsmRegisterFunc (Func (FuncDec (Ctx ctx name) _ _) _) = vsmRegisterGlobalName name ctx

vsmRegisterLocalName name ctx = do
    lregistry <- get'vsLocalRegistry
    case lregistry of
        [] -> error "no local scope on stack"
        (top:rest) -> put'vsLocalRegistry (M.insert name ctx top : rest)

vsmAddGlobalRef name ctx = do
    registry <- get'vsGlobalRegistry
    case M.lookup name registry of
        Nothing -> return ()
        Just Nothing -> return ()
        Just (Just ctx) -> do
            refs <- get'vsRefs
            put'vsRefs (M.insert (RefPos name (textLine0 $ srcTextLocation ctx) (textColumn0 $ srcTextLocation ctx)) ctx refs)

vsmAddRef name ctx = do
      lr <- get'vsLocalRegistry
      go lr
   where go [] = vsmAddGlobalRef name ctx
         go (top:rest) = do
             case M.lookup name top of
                 Nothing -> go rest
                 Just Nothing -> return ()
                 Just (Just ctx) -> do
                     refs <- get'vsRefs
                     put'vsRefs (M.insert (RefPos name (textLine0 $ srcTextLocation ctx) (textColumn0 $ srcTextLocation ctx)) ctx refs)

vsmAddGV :: Var -> VState ()
vsmAddGV var = get'vsGVs >>= put'vsGVs . (var:)
vsmAddGF :: FuncDec -> VState ()
vsmAddGF fd = get'vsGFs >>= put'vsGFs . (fd:)

vsmAddFunc :: Ctx Func -> VState ()
vsmAddFunc func = get'vsFuncs >>= put'vsFuncs . (func :)

vsmAddGlobal :: Global -> VState ()
vsmAddGlobal global = get'vsGlobals >>= put'vsGlobals . (global :)

vsmAddState :: Ctx State -> VState ()
vsmAddState state = get'vsStates >>= put'vsStates . (state :)

vsmAddLocal :: Maybe SourceContext -> Var -> VState ()
vsmAddLocal ctx v@(Var name _) = do
    locals <- get'vsLocalVars
    case locals of
        [] -> error "internal error - no local scope"
        (top:rest) ->
            if (defined name $ concat locals) || (isConstant name)
                then vsmAddErr (ctx, name ++ " is already defined")
                else do
                    put'vsLocalVars ((v:top):rest)
                    vsmRegisterLocalName name ctx

vsmAddImport imp = get'vsImports >>= put'vsImports . (imp:)

vsmAddToNamesUsed :: String -> VState ()
vsmAddToNamesUsed name = get'vsNamesUsed >>= put'vsNamesUsed . (name :)

vsmWithNewScope :: VState a -> VState a
vsmWithNewScope action = do
    get'vsLocalRegistry >>= put'vsLocalRegistry . (M.empty:)
    get'vsLabels >>= put'vsLabels . ([]:)
    get'vsLocalVars >>= put'vsLocalVars . ([]:)
    val <- action
    get'vsLocalRegistry >>= (\ (_:reg) -> put'vsLocalRegistry reg)
    get'vsLabels >>= (\ (_:labels) -> put'vsLabels labels)
    get'vsLocalVars >>= (\ (_:vars) -> put'vsLocalVars vars)
    return val

vsmWithModule :: String -> VState () -> VState ()
vsmWithModule mname action = get'vsModules >>= put'vsModules . (mname:) >> action >> get'vsModules >>= (\ (_:ms) -> put'vsModules ms)

vsmAddErr :: CodeErr -> VState ()
vsmAddErr err = do
    ctx <- get'vsContext >>= return . safeHead
    CodeErrs errs <- get'vsErr
    put'vsErr $ CodeErrs ((maybe (ctxFromCodeErr err) id ctx, msgFromCodeErr err) : errs)

vsmAddErrs :: CodeErrs -> VState ()
vsmAddErrs = mapM_ vsmAddErr . codeErrs

vsmAddLabel :: String -> VState ()
vsmAddLabel label = do
    labels <- get'vsLabels
    case labels of
        [] -> error "no local scope!"
        (top:rest) -> put'vsLabels ((label:top):rest)

vsmAddHandler handlerName = get'vsHandlersUsed >>= put'vsHandlersUsed . (handlerName:)

vsmWithContext :: Maybe SourceContext -> VState a -> VState a
vsmWithContext srcCtx action = do
    ctxs <- get'vsContext
    put'vsContext (srcCtx:ctxs)
    v <- action
    put'vsContext ctxs
    return v

vsmInEntryPoint :: LSLType -> Bool -> VState a -> VState a
vsmInEntryPoint t stateChangeAllow action = do
    put'vsEntryPointInfo (t,stateChangeAllow)
    action

vsmInBranch :: VState a -> VState a
vsmInBranch action = do
    info <- get'vsBranchReturns
    put'vsBranchReturns False
    val <- action
    put'vsBranchReturns info
    return val

vsmWithinState action = put'vsHandlersUsed [] >> action

whenM mp action = do
    p <- mp
    when p action

whenJust mv action = maybe (return ()) action mv

whenIsJust mv p action = whenJust mv (flip when action . p)

safeHead [] = Nothing
safeHead (x:_) = Just x

compileLSLScript' :: Library -> LSLScript -> Validity CompiledLSLScript
compileLSLScript' library script = evalState (compileLSLScript script) (emptyValidationState { vsLib = library })

collectLabels :: Data a => a -> [(String, Maybe SourceContext)]
collectLabels =
    everythingBut (++) (lslQ `extQ` lab)
  where
    lab :: Ctx Statement -> ([(String, Maybe SourceContext)], Bool)
    lab (Ctx ctx (Label s)) = ([(s, ctx)], False)
    lab _ = ([], False)

warnLabelsMany :: Data a => [a] -> [CodeErr]
warnLabelsMany = concatMap warnLabels

warnLabelsStates = concatMap warnLabelsState
    where warnLabelsState (Ctx _ (State _ hs)) = warnLabelsMany hs

warnLabels :: Data a => a -> [CodeErr]
warnLabels x = map (\ (name,ctx) -> (ctx,"label " ++ name ++ " is already defined (problem for LSL/Mono)")) problems
    where problems = deleteFirstsBy fstEq labels uniques
          labels = collectLabels x
          uniques = nubBy fstEq labels
          fstEq = flip ((==) . fst) . fst

compileLSLScript :: LSLScript -> VState (Validity CompiledLSLScript)
compileLSLScript (LSLScript comment globs states) = do
    preprocessGlobDefs_ "" globs
    preprocessStates states
    mapM_ vsmAddGF predefFuncs
    mapM_ compileGlob globs
    mapM_ compileState states
    err <- get'vsErr
    case err of
        CodeErrs [] -> do
           globals <- get'vsGlobals
           funcs <- get'vsFuncs
           states <- get'vsStates
           -- for now, error on label warnings since we have no warnings
           return $ case warnLabelsStates states ++ warnLabelsMany funcs of
               [] -> Right $ CompiledLSLScript comment (reverse globals) (reverse funcs) (reverse states)
               errs -> Left $ CodeErrs errs
        _ -> return . Left . CodeErrs . reverse $ codeErrs err

preprocessStates states = let snames = map (\ (Ctx _ (State cn _)) -> ctxItem cn) states in put'vsStateNames snames

preprocessGlobDefs :: String -> [GlobDef] -> VState ([Var],[FuncDec])
preprocessGlobDefs prefix globs = do
    preprocessGlobDefs_ prefix globs
    vars <- get'vsGVs
    funcDecs <- get'vsGFs
    return (vars,funcDecs)

preprocessGlobDefs_ :: String -> [GlobDef] -> VState ()
preprocessGlobDefs_ = mapM_ . preprocessGlobDef

preprocessGlobDef :: String -> GlobDef -> VState ()
preprocessGlobDef prefix (GV (Ctx ctx v@(Var name t)) _) = vsmAddGV (Var (prefix ++ name) t)
preprocessGlobDef prefix (GF (Ctx _ (Func (FuncDec name t params) _))) = vsmAddGF (FuncDec (fmap (prefix++) name) t params)
preprocessGlobDef prefix (GI moduleName _ prefix') = do
    lib <- get'vsLib
    case lookupModule (ctxItem moduleName) lib of
        Left err -> vsmAddErrs err
        Right (LModule globs _) -> mapM_ (preprocessGlobDef (prefix++prefix')) globs

compileGlob :: GlobDef -> VState ()
compileGlob (GV v mexpr) = do
    let v' = ctxItem v
    when (isConstant $ varName v') (vsmAddErr (srcCtx v, varName v' ++ " is a predefined constant"))
    namesUsed <- get'vsNamesUsed
    gvs <- get'vsGVs
    when (varName v' `elem` namesUsed) (vsmAddErr (srcCtx v, varName v' ++ " is already defined"))
    whenJust mexpr $ \ expr -> do
       let (_,gvs') = break (\ var -> varName var == varName v') gvs
       mt <- compileCtxSimple (drop 1 gvs') expr
       whenJust mt $ \ t -> when (not (varType v' `matchTypes` t)) (vsmAddErr (srcCtx v, "expression not of the correct type"))
    vsmRegisterGlobal v
    vsmAddToNamesUsed (varName v')
    vsmAddGlobal (GDecl v (fmap ctxItem mexpr))
compileGlob (GF cf@(Ctx ctx f@(Func (FuncDec name t params) statements))) =
    vsmWithNewScope $ do
        compileParams params
        vsmInEntryPoint t False $ do
            whenM ((return elem) `ap` (return $ ctxItem name) `ap` get'vsNamesUsed) (vsmAddErr (srcCtx name, ctxItem name ++ " is already defined"))
            returns <- compileStatements statements
            when (not returns && t /= LLVoid) (vsmAddErr (srcCtx name, ctxItem name ++ ": not all code paths return a value"))
            vsmRegisterFunc f
            vsmAddToNamesUsed $ ctxItem name
            vsmAddFunc cf
compileGlob (GI (Ctx ctx name) bindings prefix) =
    vsmWithModule name $ do
        let imp = (name, sort bindings, prefix)
        imports <- get'vsImports
        when (not (imp `elem` imports)) $ do
            library <- get'vsLib
            case lookupModule name library of
                Left (CodeErrs errs) -> vsmAddErrs $ CodeErrs (map (\ (_,err) -> (ctx, "module " ++ name ++ ": " ++ err)) errs)
                Right (LModule globs freevars) -> do
                   vars <- get'vsGVs
                   case validBindings vars freevars bindings of
                       Left (CodeErrs ((_,err):_)) -> vsmAddErr (ctx, err)
                       Right () -> do
                           let (vars',funcDecs') = evalState (preprocessGlobDefs "" globs) (emptyValidationState { vsLib = library })
                           let renames = bindings ++ (map (\ x -> (x,prefix ++ x)) ((map varName vars') ++ (funcNames funcDecs')))
                           vsmAddImport imp
                           vsmWithContext ctx $ mapM_ (rewriteGlob' prefix renames (map ctxItem freevars ++ vars')) globs

rewriteGlob' prefix renames vars (GF (Ctx ctx (Func (FuncDec name t params) statements))) =
    case lookup (ctxItem name) renames of
        Nothing -> vsmAddErr (srcCtx name, "can't rename " ++ ctxItem name ++ ": not found")
        Just name' -> do
            namesUsed <- get'vsNamesUsed
            if name' `elem` namesUsed
                then  vsmAddErr (srcCtx name, name' ++ " imported from module is already defined")
                else let rewrittenFunc = (Func (FuncDec (Ctx (srcCtx name) name') t params) $ rewriteStatements 0 renames statements)
                     in do  vsmAddToNamesUsed name'
                            vsmRegisterFunc rewrittenFunc
                            vsmAddFunc (Ctx ctx rewrittenFunc)
rewriteGlob' prefix renames vars (GV (Ctx ctx (Var name t)) mexpr) =
    case lookup name renames of
        Nothing -> vsmAddErr (ctx, "can't rename " ++ name ++ ": not found")
        Just name' -> do
            namesUsed <- get'vsNamesUsed
            if name' `elem` namesUsed
                then vsmAddErr (ctx, name' ++ " imported from module is already defined")
                else let rewrittenGlobVar = GDecl (nullCtx (Var name' t)) (fmap (ctxItem . (rewriteCtxExpr renames)) mexpr)
                     in do vsmAddToNamesUsed name'
                           vsmRegisterGlobal (Ctx ctx (Var name' t))
                           vsmAddGlobal rewrittenGlobVar
rewriteGlob' prefix0 renames vars (GI (Ctx ctx mName) bindings prefix) =
    do lib <- get'vsLib
       case lookupModule mName lib of
           Left (CodeErrs errs) -> vsmAddErrs $ CodeErrs (map (\ (_,err) -> (ctx, err)) errs)
           Right (LModule globs freevars) -> do
               case validBindings vars freevars bindings of
                  Left (CodeErrs ((_,err):_)) -> vsmAddErr (ctx, err)
                  Right _ -> do
                      bindings' <- mapM rewriteBinding bindings
                      let imp = (mName,sort bindings', prefix0 ++ prefix)
                      imports <- get'vsImports
                      when (not (imp `elem` imports)) $ do
                          let (vars',funcDecs') = evalState (preprocessGlobDefs "" globs) (emptyValidationState { vsLib = lib })
                          let renames = bindings' ++ map (\ x -> (x,prefix0 ++ prefix ++ x)) (map varName vars' ++ map (ctxItem . funcName) funcDecs')
                          vsmAddImport imp
                          mapM_ (rewriteGlob' (prefix0 ++ prefix) renames vars') globs
    where rewriteBinding (fv,rn) = case lookup rn renames of
                                       Nothing -> vsmAddErr (ctx, rn ++ ": not found") >> return (fv,rn)
                                       Just rn' -> return (fv,rn')

compileState :: Ctx State -> VState ()
compileState state@(Ctx _ (State nm handlers)) =
    vsmWithinState $ do
        states <- get'vsStates
        when (isJust (find (\ (Ctx _ (State x _))-> ctxItem x == ctxItem nm) states)) $
            vsmAddErr (srcCtx nm, ctxItem nm ++ " already defined")
        mapM_ compileHandler handlers
        vsmAddState state

compileHandler  (Ctx _ (Handler (Ctx ctx name) args stmts)) =
    vsmWithNewScope $ do
        used <- get'vsHandlersUsed
        if name `elem` used then vsmAddErr (ctx, name ++ " already used in this state")
                            else vsmAddHandler name
        case lookup name goodHandlers of
            Nothing -> vsmAddErr (ctx, name ++ " is not a valid handler name")
            Just types -> when (types /= map varType (ctxItems args)) $ vsmAddErr (ctx, "invalid parameter types for handler " ++ name)
        compileParams args
        vsmInEntryPoint LLVoid True $ compileStatements stmts

compileCtxSimple :: [Var] -> CtxExpr -> VState (Maybe LSLType)
compileCtxSimple gvs (Ctx ctx expr) = do
    t <- compileSimple gvs expr
    case t of
        Nothing -> vsmAddErr (ctx,"expression is not valid as an initializer for a global variable") >> return t
        _ -> return t

err ctx msg = vsmAddErr (ctx,msg) >> notype
notype = return Nothing
typeval t = return (Just t)

compileSimple :: [Var] -> Expr -> VState (Maybe LSLType)
compileSimple _ (IntLit i) = typeval LLInteger
compileSimple _ (FloatLit i) = typeval LLFloat
compileSimple _ (StringLit s) = typeval LLString
compileSimple _ (KeyLit k) = typeval LLKey
compileSimple gvs (Get (Ctx ctx name, All)) = do
    vsmAddGlobalRef name ctx
    case find (\ v -> varName v == name) gvs of
        Just (Var _ t) -> typeval t
        Nothing -> maybe (err ctx ("variable " ++ name ++ " not found"))
            typeval $ findConstType name
compileSimple _ (Get (Ctx ctx name,_)) = do
    vsmAddGlobalRef name ctx
    err ctx "can't access vector/rotation component in global variable initialization"
compileSimple _ (ListExpr []) = typeval LLList
compileSimple vars (ListExpr (e:es)) =
    maybe notype list =<< compileCtxSimple vars e
    where list t = do
            when (t == LLList) (vsmAddErr (srcCtx e, "lists cannot contain other lists"))
            compileSimple vars (ListExpr es)
compileSimple vars (VecExpr e1 e2 e3) = compileSimpleStructure vars LLVector [e1,e2,e3]
compileSimple vars (RotExpr e1 e2 e3 e4) = compileSimpleStructure vars LLRot [e1,e2,e3,e4]
compileSimple vars (Neg e) = do
    maybe notype neg =<< compileCtxSimple vars e
    where neg t | scalar t = typeval t
                | otherwise = err (srcCtx e) "operator only applicable to integers and floats in this context"
compileSimple _ e = notype

compileSimpleStructure :: [Var] -> LSLType -> [CtxExpr] -> VState (Maybe LSLType)
compileSimpleStructure _ t [] = typeval t
compileSimpleStructure vars t (e:es) =
    maybe notype f =<< compileCtxSimple vars e
    where f t' | scalar t' = compileSimpleStructure vars t es
               | otherwise = err (srcCtx e) ("literal of type " ++
                    lslTypeString t' ++  " is not a valid element of " ++
                    lslTypeString t)

compileStatement :: Ctx Statement -> VState Bool
compileStatement  (Ctx ctx (Decl var@(Var name t) expr)) = do
    vsmAddLocal ctx var
    whenJust expr $ \ expr' -> do
        mt' <- compileCtxExpr expr'
        whenJust mt' $ \ t' ->
            unless (matchTypes t t') $
                vsmAddErr (srcCtx expr',"type of expression in declaration of "
                    ++ name ++ " does not match " ++ lslTypeString t)
    get'vsBranchReturns
compileStatement  (Ctx ctx (While expr statement)) = do
    t <- compileCtxExpr expr
    vsmInBranch $ compileStatement statement
    get'vsBranchReturns
compileStatement (Ctx ctx(DoWhile statement expr)) = do
    t <- compileCtxExpr expr
    vsmInBranch $ compileStatement statement
    get'vsBranchReturns
compileStatement (Ctx ctx (For mexpr1 mexpr2 mexpr3 statement)) = do
    compileExpressions mexpr1
    compileExpressions mexpr3
    t <- compileMExpression mexpr2
    vsmInBranch $ compileStatement statement
    get'vsBranchReturns
compileStatement (Ctx ctx (If expr thenStmt elseStmt)) = do
    t <- compileCtxExpr expr
    ret1 <- vsmInBranch $ compileStatement thenStmt
    ret2 <- vsmInBranch $ compileStatement elseStmt
    returns <- get'vsBranchReturns
    put'vsBranchReturns (returns || (ret1 && ret2))
    get'vsBranchReturns
compileStatement (Ctx ctx NullStmt) = get'vsBranchReturns
compileStatement (Ctx ctx (Return Nothing)) = do
    (rtype,_) <- get'vsEntryPointInfo
    when (rtype /= LLVoid) (vsmAddErr (ctx,"function must return a value"))
    put'vsBranchReturns True
    return True
compileStatement (Ctx ctx (Return (Just expr))) = do
    t <- compileCtxExpr expr
    whenJust t $ \ t' -> do
        (rtype,_) <- get'vsEntryPointInfo
        case (t',rtype) of
           (LLString,LLKey) -> return ()
           (LLKey,LLString) -> return ()
           (LLInteger,LLFloat) -> return ()
           (x,y) | x == y -> return ()
                 | otherwise -> (vsmAddErr (ctx,"inappropriate return type for function/handler"))
    put'vsBranchReturns True
    return True
compileStatement (Ctx ctx (StateChange name)) = do
    (_,scallow) <- get'vsEntryPointInfo
    snames <- get'vsStateNames
    when (not scallow) $ vsmAddErr (ctx,"state changes not allowed from this context")
    when (not (name `elem` snames)) $ vsmAddErr (ctx,name ++ " is not a valid state")
    get'vsBranchReturns
compileStatement (Ctx ctx (Do expr)) = compileCtxExpr expr >> get'vsBranchReturns
compileStatement (Ctx ctx (Compound stmts)) = compileStatements stmts
compileStatement (Ctx ctx (Label _)) = put'vsBranchReturns False >> return False
compileStatement (Ctx ctx (Jump s)) = do
    labels <- get'vsLabels
    when (s `notElem` concat labels) $ vsmAddErr (ctx, "no such label to jump to: " ++ s)
    get'vsBranchReturns


compileStatements :: [CtxStmt] -> VState Bool
compileStatements stmts = do
    let newLabels = map (\ (Label s) -> s) $ filter isLabel (ctxItems stmts)
    vsmWithNewScope $ do
        mapM_ vsmAddLabel newLabels
        mapM_ compileStatement stmts
        get'vsBranchReturns

compileParams :: [CtxVar] -> VState ()
compileParams vs = mapM_ ( \(Ctx ctx v) -> vsmAddLocal ctx v) vs

isCastValid t t' = (t' == t) || (t,t') `elem` validCasts
   where validCasts = [(LLInteger,LLFloat), (LLFloat,LLInteger),
                      (LLInteger,LLString),(LLString,LLInteger),
                      (LLFloat,LLString),(LLString,LLFloat),
                      (LLString,LLVector),(LLVector,LLString),
                      (LLString,LLKey),(LLKey,LLString),
                      (LLRot,LLString),(LLString,LLRot),
                      (LLList,LLString),(LLString,LLList),
                      (LLFloat,LLList),(LLInteger,LLList),
                      (LLVector,LLList),(LLRot,LLList),
                      (LLKey,LLList)]

compileCtxExpr :: Ctx Expr -> VState (Maybe LSLType)
compileCtxExpr (Ctx ctx (Cast t expr)) =
   do mt <- compileCtxExpr expr
      case mt of
          Nothing -> return ()
          Just t' -> unless (isCastValid t' t) $
              vsmAddErr (ctx,"cannot cast a value of type " ++ lslTypeString t'
                  ++ " to type " ++ lslTypeString t)
      typeval t
compileCtxExpr (Ctx ctx0 (Get ((Ctx ctx name),component))) = do
   vsmAddRef name ctx
   vars <- get'vsGVs
   locals <- get'vsLocalVars
   let varList = (concat locals ++ vars)
   case (findType name varList `mplus` findConstType name,component) of
       (Nothing,_) -> err ctx  ("undefined variable or constant: " ++ name)
       (Just LLRot,All) -> typeval LLRot
       (Just LLRot,_) -> typeval LLFloat
       (Just LLVector,All) -> typeval LLVector
       (Just LLVector,S) ->
           vsmAddErr (ctx0,"s is not a valid component of a vector") >> typeval LLFloat
       (Just LLVector,_) -> typeval LLFloat
       (Just t,All) -> typeval t
       (Just t,_) -> err ctx "only vectors and rotations have components"
compileCtxExpr (Ctx ctx (Call name exprs)) = compileCall name exprs
compileCtxExpr (Ctx ctx (Not expr)) =
    do mt <- compileCtxExpr expr
       (mt `whenIsJust` (/=LLInteger)) $ vsmAddErr (ctx, "operator not applicable to non-integer type")
       typeval LLInteger
compileCtxExpr (Ctx ctx (Neg expr))  =
    do mt <- compileCtxExpr expr
       (mt `whenIsJust` (`elem` [LLList,LLString,LLKey])) $ vsmAddErr (ctx, "operator not applicable to this type")
       return mt
compileCtxExpr (Ctx ctx (Inv expr)) =
    do mt <- compileCtxExpr expr
       (mt `whenIsJust` (/=LLInteger)) $ vsmAddErr (srcCtx expr, "operator not applicable to non-integer type")
       return mt
compileCtxExpr (Ctx ctx plus@(Add expr1 expr2)) =
    do  mtypes <- compileEach (expr1,expr2)
        chkMTypes mtypes $ \ types -> case types of
            (LLInteger,LLInteger) -> typeval LLInteger
            (LLInteger,LLFloat) -> typeval LLFloat
            (LLFloat,LLInteger) -> typeval LLFloat
            (LLFloat,LLFloat) -> typeval LLFloat
            (LLVector,LLVector) -> typeval LLVector
            (LLRot,LLRot) -> typeval LLRot
            (LLString,LLString) -> typeval LLString
            (LLList,LLList) -> typeval LLList
            (t,LLList) -> typeval LLList
            (LLList,t) -> typeval LLList
            (t0,t1) -> reportIncompatibleOperands ctx t0 t1 >> notype
compileCtxExpr (Ctx ctx minus@(Sub expr1 expr2)) =
    do  mtypes <- compileEach (expr1,expr2)
        chkMTypes mtypes $ \ types -> case types of
            (LLInteger,LLInteger) -> typeval LLInteger
            (LLInteger,LLFloat) -> typeval LLFloat
            (LLFloat,LLInteger) -> typeval LLFloat
            (LLFloat,LLFloat) -> typeval LLFloat
            (LLVector,LLVector) -> typeval LLVector
            (LLRot,LLRot) -> typeval LLRot
            (t0,t1) -> reportIncompatibleOperands ctx t0 t1 >> notype
compileCtxExpr (Ctx ctx expr@(Mul expr1 expr2)) =
    do  mtypes <- compileEach (expr1,expr2)
        chkMTypes mtypes $ \ types -> case types of
            (LLInteger,LLInteger) -> typeval LLInteger
            (LLInteger,LLFloat) -> typeval LLFloat
            (LLFloat,LLInteger) -> typeval LLFloat
            (LLFloat,LLFloat) -> typeval LLFloat
            (LLVector,LLInteger) -> typeval LLVector
            (LLVector,LLFloat) -> typeval LLVector
            (LLFloat,LLVector) -> typeval LLVector
            (LLInteger,LLVector) -> typeval LLVector
            (LLVector,LLVector) -> typeval LLFloat
            (LLVector,LLRot) -> typeval LLVector
            (LLRot,LLRot) -> typeval LLRot
            (t0,t1) -> reportIncompatibleOperands ctx t0 t1 >> notype
compileCtxExpr (Ctx ctx expr@(Div expr1 expr2)) =
    do  mtypes <- compileEach (expr1,expr2)
        chkMTypes mtypes $ \ types -> case types of
            (LLInteger,LLInteger) -> typeval LLInteger
            (LLInteger,LLFloat) -> typeval LLFloat
            (LLFloat,LLInteger) -> typeval LLFloat
            (LLFloat,LLFloat) -> typeval LLFloat
            (LLVector,LLInteger) -> typeval LLVector
            (LLVector,LLFloat) -> typeval LLVector
            (LLVector,LLRot) -> typeval LLVector
            (LLRot,LLRot) -> typeval LLRot
            (t0,t1) -> reportIncompatibleOperands ctx t0 t1 >> notype
compileCtxExpr (Ctx ctx expr@(Mod expr1 expr2)) =
    do mtypes <- compileEach (expr1,expr2)
       chkMTypes mtypes $ \ types -> case types of
           (LLInteger,LLInteger) -> typeval LLInteger
           (LLVector,LLVector) -> typeval LLVector
           (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(Equal expr1 expr2)) =
    do mtypes <- compileEach (expr1,expr2)
       chkMTypes mtypes $ \ types -> case types of
           (LLInteger,LLFloat) -> typeval LLInteger
           (LLFloat,LLInteger) -> typeval LLInteger
           (LLString,LLKey) -> typeval LLInteger
           (LLKey,LLString) -> typeval LLInteger
           (t1,t2) | (t1 == t2) -> typeval LLInteger
                   | otherwise  -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(NotEqual expr1 expr2)) =
    do mtypes <- compileEach (expr1,expr2)
       chkMTypes mtypes $ \ types -> case types of
           (LLInteger,LLFloat) -> typeval LLInteger
           (LLFloat,LLInteger) -> typeval LLInteger
           (LLString,LLKey) -> typeval LLInteger
           (LLKey,LLString) -> typeval LLInteger
           (t1,t2) | (t1 == t2) -> typeval LLInteger
                   | otherwise  -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(BAnd expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(BOr expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Xor expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(ShiftL expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(ShiftR expr1 expr2)) = compileBothInteger ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Gt expr1 expr2)) = compileRelExpr ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Ge expr1 expr2)) = compileRelExpr ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Le expr1 expr2)) = compileRelExpr ctx (expr1,expr2)
compileCtxExpr (Ctx ctx e@(Lt expr1 expr2)) = compileRelExpr ctx (expr1, expr2)
compileCtxExpr (Ctx ctx e@(And expr1 expr2)) = compileBothInteger ctx (expr1, expr2)
compileCtxExpr (Ctx ctx e@(Or expr1 expr2)) = compileBothInteger ctx (expr1, expr2)
compileCtxExpr (Ctx ctx e@(IncBy (name,All) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLInteger,LLInteger) -> typeval  LLInteger
        (LLFloat,LLInteger) -> typeval  LLFloat
        (LLFloat,LLFloat) -> typeval  LLFloat
        (LLVector,LLVector) -> typeval  LLVector
        (LLRot,LLRot) -> typeval  LLRot
        (LLString,LLString) -> typeval  LLString
        (LLList,LLList) -> typeval  LLList
        (LLList,t) -> typeval  LLList
        (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(IncBy (name,_) expr) ) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (t1,t2) | structure t1 && scalar t2 -> typeval LLFloat
                | otherwise -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(DecBy (name,All) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLInteger,LLInteger) -> typeval  LLInteger
        (LLFloat,LLInteger) -> typeval  LLFloat
        (LLFloat,LLFloat) -> typeval  LLFloat
        (LLVector,LLVector) -> typeval  LLVector
        (LLRot,LLRot) -> typeval  LLRot
        (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(DecBy (name,_) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (t1,t2) | structure t1 && scalar t2 -> typeval  LLFloat
                | otherwise -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(MulBy (name,All) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLInteger,LLInteger) -> typeval  LLInteger
        (LLFloat,LLInteger) -> typeval  LLFloat
        (LLFloat,LLFloat) -> typeval  LLFloat
        (LLVector,LLInteger) -> typeval  LLVector
        (LLVector,LLFloat) -> typeval  LLVector
        (LLVector,LLVector) -> typeval  LLVector -- note: LSL compiles this, but it results in runtime error!
        (LLVector,LLRot) -> typeval  LLVector
        (LLRot,LLRot) -> typeval  LLRot
        (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(MulBy (name,_) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (t1,t2) | structure t1 && scalar t2 -> typeval  LLFloat
                | otherwise -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(DivBy (name,All) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLInteger,LLInteger) -> typeval  LLInteger
        (LLFloat,LLInteger) -> typeval  LLFloat
        (LLFloat,LLFloat) -> typeval  LLFloat
        (LLVector,LLInteger) -> typeval  LLVector
        (LLVector,LLFloat) -> typeval  LLVector
        (LLVector,LLRot) -> typeval  LLVector
        (LLRot,LLRot) -> typeval  LLRot
        (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(DivBy (name,_) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (t1,t2) | structure t1 && scalar t2 -> typeval  LLFloat
                | otherwise -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(ModBy (name,All) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLInteger,LLInteger) -> typeval  LLInteger
        (LLVector,LLVector) -> typeval  LLVector
        (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(ModBy (name,_) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ (t1,t2) ->
        reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx e@(PostInc var)) = compileIncDecOp var "++"
compileCtxExpr (Ctx ctx e@(PostDec var)) = compileIncDecOp var "--"
compileCtxExpr (Ctx ctx e@(PreInc var)) = compileIncDecOp var "++"
compileCtxExpr (Ctx ctx e@(PreDec var)) = compileIncDecOp var "++"
compileCtxExpr (Ctx ctx expr0@(Set (name,All) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLFloat,LLInteger) -> typeval  LLFloat
        (LLKey,LLString) -> typeval  LLKey
        (LLString,LLKey) -> typeval  LLString
        (t1,t2) | t1 == t2 -> typeval  t1
                | otherwise -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx expr0@(Set (name,S) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLRot,LLFloat) -> typeval  LLFloat
        (LLRot,LLInteger) -> typeval  LLFloat
        (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx expr0@(Set (name,_) expr)) = do
    reportErrorIfNoModify name
    mtypes <- compileNameExpr (name,expr)
    chkMTypes mtypes $ \ types -> case types of
        (LLVector,LLFloat) -> typeval  LLFloat
        (LLVector,LLInteger) -> typeval  LLFloat
        (LLRot,LLFloat) -> typeval  LLFloat
        (LLRot,LLInteger) -> typeval  LLFloat
        (t1,t2) -> reportIncompatibleOperands ctx t1 t2 >> notype
compileCtxExpr (Ctx ctx (IntLit i)) = typeval  LLInteger
compileCtxExpr (Ctx ctx (FloatLit _)) = typeval  LLFloat
compileCtxExpr (Ctx ctx (StringLit _)) = typeval  LLString
compileCtxExpr (Ctx ctx (KeyLit _)) = typeval  LLKey
compileCtxExpr (Ctx ctx (ListExpr es)) = do
    mapM compileListExprElement es
    typeval  LLList
compileCtxExpr (Ctx ctx (VecExpr xExpr yExpr zExpr)) =
    do  xt <- compileCtxExpr xExpr
        yt <- compileCtxExpr yExpr
        zt <- compileCtxExpr zExpr
        when (not (all (`elem` [LLInteger,LLFloat]) [c | Just c <- [xt,yt,zt]])) $ vsmAddErr (ctx, "invalid components for vector")
        typeval  LLVector
compileCtxExpr (Ctx ctx (RotExpr xExpr yExpr zExpr sExpr)) =
    do  xt <- compileCtxExpr xExpr
        yt <- compileCtxExpr yExpr
        zt <- compileCtxExpr zExpr
        st <- compileCtxExpr sExpr
        when (not (all (`elem` [LLInteger,LLFloat]) [ c | Just c <- [xt,yt,zt,st]])) $ vsmAddErr (ctx, "invalid components for rotation")
        typeval LLRot

chkMTypes (Nothing,Nothing) _ = notype
chkMTypes (Nothing,Just t) _ = typeval t
chkMTypes (Just t,Nothing) _ = typeval t
chkMTypes (Just t1,Just t2) chkTypes = chkTypes (t1,t2)

reportErrorIfNoModify (Ctx ctx name) =
    when (isConstant name) $ vsmAddErr (ctx, "cannot modify " ++ name ++ " because it is a constant")

reportIncompatibleOperands ctx t0 t1 =
    vsmAddErr (ctx,"the types of the operands aren't compatible (" ++ lslTypeString t0 ++ " vs. " ++ lslTypeString t1 ++ ")")

compileExpressions es = mapM_ compileCtxExpr es
compileMExpression Nothing = typeval LLVoid
compileMExpression (Just expr) = compileCtxExpr expr

compileCall (Ctx ctx fname) exprs = do
    vsmAddRef fname ctx
    funcs <- get'vsGFs
    case findFuncDec fname funcs of
        Nothing -> vsmAddErr (ctx,fname ++ ": no such function in scope") >> notype
        Just (FuncDec _ t params) ->
            let vArg :: Int -> [Var] -> [Ctx Expr] -> VState ()
                vArg _ [] [] = return ()
                vArg _ (p:ps) [] = vsmAddErr (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
                vArg _ [] (a:as) = vsmAddErr (ctx, "mismatch of arguments vs. formal paramters in call to function " ++ fname)
                vArg n (Var name t:ts) (arg:args) =
                  do mt' <- compileCtxExpr arg
                     case mt' of
                         Nothing -> return ()
                         Just t' -> when (not (matchTypes t t')) $ vsmAddErr (ctx, "argument " ++ (show n) ++ " in call to function (" ++ fname ++ ") is of wrong type:" ++ (lslTypeString t') ++ ", should be " ++ (lslTypeString t))
                     vArg (n+1) ts args
            in vArg 1 (ctxItems params) exprs >> typeval t

compileEach :: (Ctx Expr,Ctx Expr) -> VState (Maybe LSLType, Maybe LSLType)
compileEach (expr1,expr2) = do
    mt1 <- compileCtxExpr expr1
    mt2 <- compileCtxExpr expr2
    return (mt1,mt2)
compileBothInteger ctx (expr1,expr2) = do
    (mt1,mt2) <- compileEach (expr1,expr2)
    case (mt1,mt2) of
        (Just LLInteger,Just LLInteger) -> typeval LLInteger
        (Just _, Just _) -> vsmAddErr (ctx,"operands are of incompatible type") >> notype
        _ -> notype
compileRelExpr ctx (expr1,expr2) = do
    do (mt1,mt2) <- compileEach (expr1,expr2)
       case (mt1,mt2) of
           (Nothing,_) -> notype
           (_,Nothing) -> notype
           (Just LLInteger,Just LLInteger) -> typeval LLInteger
           (Just LLInteger,Just LLFloat) -> typeval LLInteger
           (Just LLFloat,Just LLInteger) -> typeval LLInteger
           (Just LLFloat,Just LLFloat) -> typeval LLInteger
           (Just t0,Just t1) -> vsmAddErr (ctx,"operands are of incompatible types") >> notype
compileNameExpr (Ctx ctx name,expr) = do
    vsmAddRef name ctx
    locals <- get'vsLocalVars
    globvars <- get'vsGVs
    mt <- compileCtxExpr expr
    let mvt = findType name (concat locals ++ globvars)
    when (isNothing mvt) $ vsmAddErr (ctx,"variable " ++ name ++ " not defined")
    return (mvt,mt)

compileIncDecOp (n@(Ctx ctx name),c) op = do
    vsmAddRef name ctx
    reportErrorIfNoModify n
    locals <- get'vsLocalVars
    vars <- get'vsGVs
    case (findType name (concat locals ++ vars),c) of
        (Nothing,_) ->  vsmAddErr (ctx, "variable " ++ name ++ " not found") >> notype
        (Just LLInteger,All) -> typeval LLInteger
        (Just LLFloat,All) -> typeval LLFloat
        (Just LLRot,S) -> typeval LLFloat
        (Just LLVector,S) -> vsmAddErr (ctx, "s is not a valid component of " ++ name) >> notype
        (Just t,All) -> vsmAddErr (ctx, name ++ " is not a valid operand for " ++ op) >> notype
        (Just LLVector,_) -> typeval LLFloat
        (Just LLRot,_) -> typeval LLFloat
        _ -> vsmAddErr (ctx, name ++ " is not a valid operand for " ++ op) >> notype

compileListExprElement e@(Ctx ctx _) = do
    mt <- compileCtxExpr e
    case mt of
        Nothing -> return ()
        Just t | t `elem` [LLVoid,LLList] -> vsmAddErr (ctx,"invalid list element")
               | otherwise -> return ()

validBindings vars freevars bindings =
    if length freevars /= length bindings then
        throwStrError ("wrong number of bindings in import: " ++ (show $ length freevars) ++ " required")
    else let f [] = return ()
             f ((x,y):xys) =
                case (findType x (ctxItems freevars), findType y vars) of
                    (Nothing,_) -> throwStrError ("free variable " ++ x ++ " not found")
                    (_,Nothing) -> throwStrError ("global variable " ++ y ++ " not found")
                    (Just t0,Just t1) | not (matchTypes t0 t1) -> throwStrError ("types of " ++ x ++ " and " ++ y ++ " don't match")
                            | otherwise -> f xys
         in f bindings

defined :: String -> [Var] -> Bool
defined n = any (\ (Var n' _) -> n == n')

-- Validating a library of modules

compileModule :: LModule -> VState (Validity ([Global],[Ctx Func]))
compileModule m@(LModule globs freevars) = do
    mapM_ (vsmAddGV . ctxItem) freevars
    preprocessGlobDefs_ "" globs
    mapM_ vsmAddGF predefFuncs
    mapM_ compileGlob globs
    errs <- get'vsErr
    case errs of
        CodeErrs [] -> do
            globals <- get'vsGlobals
            funcs <- get'vsFuncs
            return $ Right (globals,funcs)
        _ -> return $ Left errs

-- this function isn't partiuclarly efficient!
moduleDependencies :: [(String,LModule)] -> [String] -> String -> Validity [String]
moduleDependencies lib chain m =
    let f (GI s _ _) = Just (ctxItem s)
        f _          = Nothing
    in do
        (LModule globs _) <- lookupM m lib
        case filtMap f globs of
            [] -> return []
            list -> if any (`elem` list) (m:chain)
                then throwStrError "circular dependency"
                else do
                    deps <- fmap concat (
                        let chain' = (m:chain) in
                                mapM (moduleDependencies lib chain') list)
                    return $ nub (list ++ deps)

-- sort modules by dependency: for each module in the list, after sorting that module
-- will depend only on modules preceding it in the list.  This of course implies that
-- there can be no circular dependencies in the modules.
sortModules :: [(String,(LModule,[String]))] -> [(String,LModule)]
sortModules modules =
    let cmp (name,(_,deplist)) (name',(_,deplist')) =
            compare (length deplist, name) (length deplist', name')
        sort1 [] = []
        sort1 list =
           let sorted = sortBy cmp list
               (nodeps,deps) = span ((==0).length.snd.snd) sorted
               exclude = map fst nodeps
               newlist = if length nodeps == 0 then error "circular depencencies in library"
                         else map (\ (nm,(m,l)) -> (nm, (m,filter (`notElem` exclude) l))) deps
           in nodeps ++ sort1 newlist
   in map (\ (s,(m,_)) -> (s,m)) $ sort1 modules

compileLibrary :: [(String,LModule)] -> [(String,Validity (LModule,ModuleInfo))]
compileLibrary modules =
    let checkDep (n,m) = case moduleDependencies modules [] n of
            Right deps -> (n,Right (m,deps))
            Left s -> (n,Left s)
        categorize (good,bad) (n,Left s) = (good,(n,s):bad)
        categorize (good,bad) (n,Right (m,deps)) = ((n,(m,deps)):good,bad)
        (good,bad) = foldl categorize ([],[]) $ map checkDep modules
        sorted = sortModules good
        validate augLib (name,m) =
            case evalState (compileModule m) (emptyValidationState { vsLib = libFromAugLib augLib })  of
                Left s -> (name, Left s):augLib
                Right gs -> (name,Right (m,gs)):augLib
    in (foldl validate [] sorted) ++ (map (\ (n,s) -> (n,Left s)) bad)

libFromAugLib :: AugmentedLibrary -> Library
libFromAugLib augLib =
   let f (name,Left s) = (name,Left s)
       f (name,Right (lm,_)) = (name, Right lm)
   in map f augLib

-- | Transform a script into a module, generating function names for each handler, using the
-- pattern <state-name>_state_<handler-name>.
-- The script following script:
--
--  default {
--      state_entry() {
--          llSay(0,"Hello Avatar");
--      }
--  }
--
--  would be transformed into a module equivalent to:
--
--  $module
--
--  default_state_state_entry() {
--      llSay(0,"Hello Avatar");
--  }
moduleFromScript :: CompiledLSLScript -> LModule
moduleFromScript script = LModule globDefs []
    where globDefs = globDefsFromGlobs (scriptGlobals script) ++
                     globDefsFromFuncs (scriptFuncs script) ++
                     funcDefsFromStates (scriptStates script)
          globDefsFromGlobs = map globDefFromGlob
          globDefsFromFuncs = map GF
          funcDefsFromStates = concatMap funcDefsFromState

globDefFromGlob (GDecl v me) = GV v (fmap nullCtx me)
funcDefsFromState (Ctx _ (State ctxnm handlers)) = map (funcDefFromHandler (ctxItem ctxnm)) handlers
funcDefFromHandler stateName (Ctx _ (Handler ctxnm params stmts)) = GF $ nullCtx $ Func (FuncDec combinedName LLVoid params) stmts
    where combinedName = nullCtx $ stateName ++ "$$" ++ (ctxItem ctxnm)

rewriteCtxExpr :: [(String,String)] -> Ctx Expr -> Ctx Expr
rewriteCtxExpr renames = everywhere (mkT (rewriteName renames))

rewriteName :: [(String,String)] -> Ctx String -> Ctx String
rewriteName renames (Ctx ctx name) =
    case lookup name renames of
        Nothing -> Ctx ctx name
        Just name' -> Ctx ctx name'

rewriteCtxStatement n bindings (Ctx c s) =
    let (n',bindings',s') = rewriteStatement n bindings s in (n',bindings', Ctx c s')

rewriteStatements _ _ [] = []
rewriteStatements n bindings (Ctx c s:ss) =
    let (n',bindings',s') = rewriteStatement n bindings s in
       (Ctx c s'):(rewriteStatements n' bindings' ss)

rewriteStatement n bindings (Compound stmts) = (n, bindings, Compound $ rewriteStatements n bindings stmts)
rewriteStatement n bindings (While expr stmt) =
    let (_,_,stmt') = rewriteCtxStatement n bindings stmt in
    (n, bindings, While (rewriteCtxExpr bindings expr) stmt')
rewriteStatement n bindings (DoWhile stmt expr) =
    let (_,_,stmt') = rewriteCtxStatement n bindings stmt in
    (n, bindings, DoWhile stmt' (rewriteCtxExpr bindings expr))
rewriteStatement n bindings (For mexpr1 mexpr2 mexpr3 stmt) =
    let (_,_,stmt') = rewriteCtxStatement n bindings stmt
        rewriteMExpr = rewriteMExpression bindings
        rewriteEs = rewriteCtxExprs bindings in
    (n, bindings, For (rewriteEs mexpr1) (rewriteMExpr mexpr2) (rewriteEs mexpr3) stmt')
rewriteStatement n bindings (If expr stmt1 stmt2) =
    let (_,_,stmt1') = rewriteCtxStatement n bindings stmt1
        (_,_,stmt2') = rewriteCtxStatement n bindings stmt2 in
        (n, bindings, If (rewriteCtxExpr bindings expr) stmt1' stmt2')
rewriteStatement n bindings (Decl (Var name t) val) =
   let (n',bindings', newname) =
           if any (\(name',_) -> name == name') bindings then let newname = "local" ++ (show n) in (n + 1, (name,newname):bindings, newname)
           else (n,bindings,name)
   in (n',bindings',Decl (Var newname t) (rewriteMExpression bindings val))
rewriteStatement n bindings (Return Nothing) = (n, bindings, Return Nothing)
rewriteStatement n bindings (Return (Just expr)) = (n, bindings, Return $ Just $ rewriteCtxExpr bindings expr)
rewriteStatement n bindings (Do expr) = (n, bindings, Do $ rewriteCtxExpr bindings expr)
rewriteStatement n bindings s = (n, bindings, s)

rewriteCtxExprs bindings ctxExprs = map (rewriteCtxExpr bindings) ctxExprs

rewriteMExpression bindings = fmap (rewriteCtxExpr bindings)

scalar = (`elem` [LLFloat,LLInteger])
structure = (`elem` [LLRot,LLVector])

-- | Base generics query for traversing lsl
lslQ :: Typeable a => a -> ([r], Bool)
lslQ = ([], False) `mkQ` string `extQ` sctx
  where
    sctx   = stop :: SourceContext -> ([r], Bool)
    string = stop :: String -> ([r], Bool)
    stop   = const ([], True)
