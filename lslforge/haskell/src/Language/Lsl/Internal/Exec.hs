{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             NoMonomorphismRestriction,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.Exec(
    ScriptImage(..),
    EvalState,
    ExecutionInfo(..),
    ExecutionState(..),
    FrameInfo(..),
    MemRegion,
    Binding,
    executeLsl,
    frameInfo,
    initLSLScript,
    initStateSimple,
    setupSimple,
    evalSimple,
    emptyMemRegion,
    runEval,
    scriptImage,
    softReset,
    hardReset,
    hasActiveHandler) where

--import Debug.Trace
import Data.Bits((.&.),(.|.),xor,shiftL,shiftR,complement)
import Data.List(intersperse,find,tails)
import Data.Data
import qualified Data.Map as M
import Data.Maybe(isJust)

import Language.Lsl.Internal.Breakpoint(Breakpoint(..),StepManager(..),
    pushStepManagerFrame,popStepManagerFrame,emptyStepManager,mkBreakpoint)
import Language.Lsl.Internal.CodeHelper(renderCall)
import Language.Lsl.Internal.FuncSigs(convertArgs)
import Language.Lsl.Internal.Util(fromInt,lookupM,ctx,findM)
import Language.Lsl.Syntax(
    Expr(..), CompiledLSLScript(..),Statement(..),Func(..),FuncDec(..),Var(..),
    State(..),Ctx(..),Global(..),TextLocation(..),SourceContext(..),
    Handler(..),ctxVr2Vr,findFunc,findFuncDec,ctxItems,findState,fromMCtx,
    predefFuncs)
import Language.Lsl.Internal.Type(
    LSLType(..),LSLValue(..),typeOfLSLComponent,typeOfLSLValue,toFloat,toSVal,
    lslShowVal,replaceLslValueComponent,vecMulScalar,rotMulVec,parseVector,
    parseRotation,parseInt,parseFloat,invRot,rotMul,vcross,Component(..),
    lslValueComponent)
import Language.Lsl.Internal.Key(nullKey,nextKey,LSLKey(..))
import Language.Lsl.Internal.Evaluation(EvalResult(..),Event(..),
    ScriptInfo(..))
import Language.Lsl.Internal.Constants(findConstVal,llcZeroRotation,
    llcZeroVector)

import Control.Applicative
import Control.Monad(foldM_,when,mplus,msum,zipWithM,ap,liftM)
import Control.Monad.State(MonadState(..),lift,StateT(..))
import Control.Monad.Error(MonadError(..),ErrorT(..))
import Control.Monad.Trans

--import Debug.Trace(trace)

-- initialize a script for execution
initLSLScript :: RealFloat a => CompiledLSLScript -> ScriptImage a
initLSLScript (CompiledLSLScript _ globals fs ss)  =
    ScriptImage {
        scriptImageName = "",
        curState = "default",
        executionState = Waiting,
        glob = initGlobals globals,
        funcs = map ctxItem fs,
        states = map ctxItem ss,
        valueStack = [],
        callStack = [],
        stepManager = emptyStepManager,
        globals = globals,
        currentEvent = Nothing }

initStateSimple script perfAction log qtick utick chkBp =
    EvalState { scriptImage = initLSLScript script,
                objectId = nextKey nullKey,
                primId = 0,
                scriptName = "script",
                myPrimKey = nextKey nullKey,
                performAction = perfAction,
                logMessage = log,
                qwtick = qtick,
                uwtick = utick,
                checkBreakpoint = chkBp,
                nextEvent = undefined }

runEval = (runStateT . runErrorT . unEval)

executeLsl img oid pid sid pkey perf log qtick utick chkBp queue maxTick =
     do let state = (EvalState { scriptImage = img,
                            objectId = oid,
                            primId = pid,
                            scriptName = sid,
                            myPrimKey = pkey,
                            performAction = perf,
                            logMessage = log,
                            qwtick = qtick,
                            uwtick = utick,
                            checkBreakpoint = chkBp,
                            nextEvent = undefined})
        result <- (runEval $ evalScript maxTick queue) state
        case result of
            (Left s,_) -> return $ Left s
            (Right queue',evalState) ->
                return $ Right (scriptImage evalState, queue')

-- The state of evaluation for a script.
data EvalState m a = EvalState {
     scriptImage :: ScriptImage a,
     objectId :: LSLKey,
     primId :: Int,
     scriptName :: String,
     myPrimKey :: LSLKey,
     performAction :: String -> ScriptInfo a -> [LSLValue a] ->
        m (EvalResult,LSLValue a),
     logMessage :: String -> m (),
     qwtick :: m Int,
     uwtick :: Int -> m (),
     checkBreakpoint :: Breakpoint -> StepManager -> m (Bool, StepManager),
     nextEvent :: String -> String -> m (Maybe (Event a))  }

--type Eval m a = ErrorT String (StateT (EvalState m a) m)
newtype Eval a m b = Eval { unEval :: ErrorT String (StateT (EvalState m a) m) b }
    deriving (Monad)

instance Monad m => MonadState (EvalState m a) (Eval a m) where
   get = Eval { unEval = get }
   put v = Eval { unEval = put v }

instance MonadTrans (Eval a) where
    lift v = Eval { unEval = lift $ lift v }

instance Monad m => MonadError String (Eval a m) where
    throwError e = Eval { unEval = throwError e }
    catchError v f = Eval { unEval = catchError (unEval v) (unEval . f) }

instance Monad m => Functor (Eval a m) where
    fmap = liftM

instance Monad m => Applicative (Eval a m) where
   pure  = return
   (<*>) = ap

-- the image, or 'memory state' of a running script.  Includes the
-- status of all stacks, global variables, and immutable items like
-- the functions and state definitions.
data ScriptImage a = ScriptImage {
    scriptImageName :: String,
    curState :: StateName,
    executionState :: ExecutionState,
    glob :: MemRegion a,
    funcs :: [Func],
    states :: [State],
    valueStack :: ValueStack a,
    callStack :: CallStack a,
    stepManager :: StepManager,
    globals :: [Global],
    currentEvent :: Maybe (Event a)
    } deriving (Show)

data FrameInfo a = FrameInfo {
    frameInfoImageName :: String,
    frameInfoFrames :: [(String,Maybe SourceContext,Maybe Int,[(String,LSLValue a)])] }
    deriving (Show)

frameInfo scriptImage = FrameInfo (scriptImageName scriptImage) $
    frames ++ [("glob", bottomContext, Nothing, glob scriptImage)]
    where frames = (map collapseFrame $ callStack scriptImage)
          (bottomContext,_) = case frames of
              [] -> (Nothing,Nothing)
              _ -> let (_,ctx,_,_) = last frames in (ctx,Just 1)
          collapseFrame (Frame name ctx line (ss,_)) =
              (name,ctx,line,concat $ map scopeMem ss)
-- a soft reset occurs when a script that has been running, but
-- has been persisted to inventory, is reactivated.  The curState
-- stays the same, but if the script was running or sleeping, the
-- script goes to the 'waiting' state, with its call stack and value
-- stack cleared.

-- if a script was halted, it stays halted; if it was waiting, it
-- stays waiting; if it was erroneous, it stays erroneous
softReset image =
   let image' = image { valueStack = [], callStack = [] } in
       case executionState image' of
           SleepingTil _ -> image' { executionState = Waiting }
           Executing -> image' { executionState = Waiting }
           _ -> image'

-- a hard reset occurs when a script is explicitly reset, either
-- via an LSL call, or 'manually' (in the real SL, via the GUI)
hardReset image =
    case executionState image of
        Erroneous _ -> image -- no effect
        Halted -> image -- halted scripts are scripts that are explicitly not in the run state
        _ -> image {
            curState = "default",
            executionState = Waiting,
            glob = initGlobals (globals image),
            valueStack = [],
            callStack = [] }

type StateName = String

-- a labeled block: a label and all the statements that follow it.
data LBlock = LBlock String [Ctx Statement] deriving (Show)

-- find a block in a list of labeled blocks
findLBlock name = find (\ (LBlock n _) -> name == n)

-- label the blocks in a list of statements `(i.e.
-- (EXAMPLE:
--        {
--           @foo;
--           if (i == 7) jump bar;
--           @bar;
--           llSay(0, (string) (i + j));
--           if (j == 10) jump foo;
--        }
--  In the above code, there are two labels, and there for two labelled
--  blocks.  The first is 'foo', labelling the block:
--           if (i == 7) jump bar;
--           @bar;
--           llSay(0, (string) (i + j));
--           if (j == 10) jump foo;
--
--  The 2nd is 'bar', labelling the block:
--           llSay(0, (string) (i + j));
--           if (j == 10) jump foo;
--
--labelBlocks [] = []
-- labelBlocks (ctxStmt:stmts) =
--     case ctxItem ctxStmt of
--         Label s -> (LBlock s stmts):(labelBlocks stmts)
--         _ -> (labelBlocks stmts)

labelBlocks ss =
    [ LBlock s stmts | (Ctx { ctxItem = Label s}:stmts) <- tails ss]

-- A name/element pair is the basic unit of memory.  Each memory
-- location has a name (rather than an address) and can hold a value
-- which can be of any LSL type.  So 1 memory location can hold a
-- vector, or a string, or a list, etc.
type Binding a = (String,LSLValue a)

-- a memory region is just a collection of NameElementPairs
type MemRegion a = [Binding a]
emptyMemRegion = []

initVar name LLInteger Nothing = (name,IVal 0)
initVar name LLFloat Nothing  = (name,FVal $ realToFrac 0.0)
initVar name LLString Nothing = (name,SVal "")
initVar name LLKey Nothing = (name,SVal "")
initVar name LLList Nothing = (name,LVal [])
initVar name LLVector Nothing = (name,VVal 0.0 0.0 0.0)
initVar name LLRot Nothing = (name,RVal 0.0 0.0 0.0 1.0)
initVar name LLKey (Just (SVal s)) = (name,KVal $ LSLKey s)
initVar name LLString (Just (KVal k)) = (name, SVal $ unLslKey k)
initVar name LLFloat (Just (IVal i)) = (name,FVal $ fromInt i)
initVar name LLInteger (Just (FVal f)) = (name,IVal $ floor f)
initVar name _ (Just v) = (name,v)

writeMem :: Monad m => String -> LSLValue a -> MemRegion a -> m (MemRegion a)
writeMem name value cells =
    case break (\(name',element) -> name' == name) cells of
        (cells',[]) -> fail "no such variable"
        (xs,y:ys) -> return ((name,value):(xs ++ ys))

readMem :: Monad m => String -> MemRegion a -> m (LSLValue a)
readMem = lookupM

initGlobals :: RealFloat a => [Global] -> MemRegion a
initGlobals globals = map (initGlobal globals) globals

initGlobal globals (GDecl (Ctx _ (Var name t)) mexpr) =
    initVar name t $ fmap (evalLit globals) mexpr

evalCtxLit globals (Ctx _ expr) = evalLit globals expr

evalLit globals expr =
    let litExpr2Float expr =
            case evalCtxLit globals expr of
                (FVal f) -> f
                (IVal i) -> fromInteger $ toInteger i
                _ -> error "invalid float expression"
    in case expr of
        Neg (Ctx _ (IntLit i)) -> IVal (-i)
        Neg (Ctx _ (FloatLit f)) -> FVal (-(realToFrac f))
        IntLit i        -> IVal i
        FloatLit f      -> FVal (realToFrac f)
        StringLit s     -> SVal s
        KeyLit k        -> KVal $ LSLKey k
        ListExpr l      -> LVal $ map (evalCtxLit globals) l
        VecExpr a b c   -> VVal (litExpr2Float a) (litExpr2Float b) (litExpr2Float c)
        RotExpr a b c d -> RVal (litExpr2Float a) (litExpr2Float b) (litExpr2Float c) (litExpr2Float d)
        Get (Ctx _ nm,All)    ->
            case find (\ (GDecl (Ctx _ (Var nm' t)) mexpr') -> nm == nm') globals of
                Nothing ->
                    case findConstVal nm of
                        Nothing -> error ("invalid global " ++ nm ++ " referenced in initializer")
                        Just v -> v
                Just g ->
                    let (_,v) = initGlobal globals g in v

bindParm (Var name t) lslVal = if typeOfLSLValue lslVal == t then Just (name,lslVal) else Nothing
bindParms vars vals = zipWithM bindParm vars vals

bindParmForgiving (Var name t) lslVal =
    case (t,typeOfLSLValue lslVal) of
        (LLInteger,LLFloat) -> let FVal v = lslVal in return (name,IVal $ floor v)
        (LLFloat,LLInteger) -> let IVal v = lslVal in return (name,FVal $ fromInt v)
        (LLKey,LLString) -> let SVal s = lslVal in return (name,KVal $ LSLKey s)
        (LLString,LLKey) -> let KVal k = lslVal in return (name,SVal $ unLslKey k)
        (t0,t1) | t0 == t1 -> return (name,lslVal)
                | otherwise -> throwError "type mismatch!"

bindParmsForgiving vars vals = zipWithM bindParmForgiving vars vals

toBool x = x /= 0

--type Frame = (ScopeStack,EvalStack)
data Frame a = Frame {
    frameName :: String,
    frameContext :: Maybe SourceContext,
    frameSourceLine :: Maybe Int,
    frameStacks :: (ScopeStack a, EvalStack) }
    deriving (Show)
type LabelSet = [LBlock]
type ScopeStack a = [Scope a]
type CallStack a = [Frame a]

data Scope a = Scope {
    scopeMem :: !(MemRegion a),
    scopeLabels :: !LabelSet,
    scopeMarks :: Int }
    deriving (Show)

readVarScope :: String -> Scope a -> Maybe (LSLValue a)
readVarScope name (Scope { scopeMem = mem }) = readMem name mem
readVarSStack :: String -> ScopeStack a -> Maybe (LSLValue a)
readVarSStack name ss = foldl mplus Nothing $ map (readVarScope name) ss
readVarFrame :: String -> Frame a -> Maybe (LSLValue a)
readVarFrame name = (readVarSStack name) . fst . frameStacks
readVarCallStack :: String -> CallStack a -> Maybe (LSLValue a)
readVarCallStack name = (readVarFrame name) . head

writeVarScope :: String -> LSLValue a -> Scope a -> Maybe (Scope a)
writeVarScope name val s@(Scope { scopeMem = mem} ) = writeMem name val mem >>= \ mem' -> return s { scopeMem = mem' }
writeVarSStack rs name val [] = Nothing
writeVarSStack rs name val (s:ss) =
    case writeVarScope name val s of
        Nothing -> writeVarSStack (s:rs) name val ss
        Just s' -> Just $ (reverse rs) ++ (s':ss)
writeVarFrame name val frame =
    let (ss,es) = frameStacks frame in
    case writeVarSStack [] name val ss of
        Nothing -> Nothing
        Just ss' -> Just frame { frameStacks = (ss',es) }
writeVarCallStack name val (frame:cs) = (flip (:) cs) <$> writeVarFrame name val frame

type EvalStack = [EvalElement]

type ValueStack a = [LSLValue a]

data EvalElement =
      EvBlock [Ctx Statement] | EvCtxStatement (Ctx Statement)
    | EvStatement Statement | EvExpr Expr | EvMexpr (Maybe Expr) | EvAdd
    | EvSub | EvMul | EvDiv | EvMod | EvBAnd | EvBOr  | EvBXor | EvBInv | EvNeg
    | EvNot | EvAnd | EvOr  | EvLe | EvLt  | EvGe   | EvGt  | EvEq | EvNe
    | EvShiftL | EvShiftR | EvCast LSLType | EvGet (String,Component)
    | EvSet (String,Component) | EvCons | EvMkVec | EvMkRot | EvPop | EvReturn
    | EvDiscard | EvBind String LSLType | EvCond (Ctx Statement) (Ctx Statement)
    | EvCall String (Maybe SourceContext) [Var] [Ctx Statement] Bool
    | EvPredef String | EvLoop Expr [Ctx Expr] (Ctx Statement) | EvMark
    deriving (Show,Data,Typeable)

queryState q = q <$> get
updateState :: Monad w => (EvalState w a -> EvalState w a) -> Eval a w ()
updateState u = put =<< u <$> get
queryExState q = queryState (q . scriptImage)
updateExState :: Monad w => (ScriptImage a -> ScriptImage a) -> Eval a w ()
updateExState u = updateState (\s -> s { scriptImage = u $ scriptImage s })

getTick :: Monad w => Eval a w Int
getTick = lift =<< qwtick <$> get
setTick :: (Monad w) => Int -> Eval a w ()
setTick v = lift =<< (uwtick <$> get <*> pure v)

doAction name scriptInfo args =
    lift =<< performAction <$> get <*> pure name <*> pure scriptInfo <*> pure args
logMsg s = lift =<< logMessage <$> get <*> pure s

checkBp bp = do
    (result,sm') <- lift =<< checkBreakpoint <$> get <*> pure bp <*> getStepManager
    setStepManager sm'
    return result

getGlob :: Monad w => Eval a w (MemRegion a)
getGlob = queryExState glob
getFuncs :: Monad w => Eval a w [Func]
getFuncs = queryExState funcs
getVStack :: Monad w => Eval a w (ValueStack a)
getVStack = queryExState valueStack
getCallStack :: Monad w => Eval a w (CallStack a)
getCallStack = queryExState callStack
getStates :: Monad w => Eval a w [State]
getStates = queryExState states
getCurrentEvent :: Monad w => Eval a w (Maybe (Event a))
getCurrentEvent = queryExState currentEvent
getExecutionState :: Monad w => Eval a w ExecutionState
getExecutionState = queryExState executionState
getCurState :: Monad w => Eval a w String
getCurState = queryExState curState
getObjectId :: Monad w => Eval a w LSLKey
getObjectId = queryState objectId
getPrimId :: Monad w => Eval a w Int
getPrimId = queryState primId
getScriptName :: Monad w => Eval a w String
getScriptName = queryState scriptName
getMyPrimKey :: Monad w => Eval a w LSLKey
getMyPrimKey = queryState myPrimKey
getStepManager :: Monad w => Eval a w StepManager
getStepManager = queryExState stepManager

setGlob g = updateExState (\e -> e { glob = g })
setVStack v = updateExState (\e -> e { valueStack = v })
setCallStack c = updateExState (\e -> e { callStack = c })
setStepManager m = updateExState (\e -> e { stepManager = m })
setExecutionState state = updateExState (\e -> e { executionState = state })
setCurState state = updateExState (\e -> e { curState = state })
setCurrentEvent event = updateExState (\e -> e { currentEvent = Just event })
setScriptImageName n = updateExState (\ e -> e { scriptImageName = n })

initStacks :: Monad w => Eval a w ()
initStacks = setVStack [] >> setCallStack []

popScope :: Monad w => Eval a w (Scope a)
popScope = do
    (frame:frames) <- getCallStack
    let (s:ss,es) = frameStacks frame
    setCallStack (frame { frameStacks = (ss,es) }:frames)
    return s

pushScope :: Monad w => MemRegion a -> LabelSet -> Eval a w ()
pushScope mem labels = do
    (frame:frames) <- getCallStack
    let (ss,es) = frameStacks frame
    setCallStack (frame { frameStacks = (Scope { scopeMem = mem, scopeLabels = labels, scopeMarks = 0}:ss,es) }:frames)

pushVal value = (setVStack . (value:)) =<< getVStack

popVal :: Monad w => Eval a w (LSLValue a)
popVal = do
    vstack <- getVStack
    case vstack of
       [] -> throwError "empty value stack"
       (v:vs) -> do
           setVStack vs
           return v

peekVal :: Monad w => Eval a w (LSLValue a)
peekVal = do
    vstack <- getVStack
    case vstack of
        [] -> throwError "empty value stack"
        (v:_) -> return v

valStackEmpty :: (RealFloat a, Read a, Monad w) => Eval a w Bool
valStackEmpty = (==[]) <$> getVStack

elementStackEmpty :: Monad w => Eval a w Bool
elementStackEmpty =
    do (frame:cs) <- getCallStack
       case frameStacks frame of
           (_,[]) -> return True
           _ -> return False

popElement :: Monad w => Eval a w EvalElement
popElement = do
    (frame:frames) <- getCallStack
    let (ss,e:es) = frameStacks frame
    setCallStack (frame { frameStacks = (ss,es) }:frames)
    return e
getEStack :: Monad w => Eval a w [EvalElement]
getEStack = do
    (frame:frames) <- getCallStack
    return $ snd $ frameStacks frame

popMarks 0 = return ()
popMarks n = do
    e <- popElement
    case e of
       EvMark -> popMarks (n - 1)
       _ -> popMarks n

pushElement element =
   do (frame:frames) <- getCallStack
      let (ss,es) = frameStacks frame
      setCallStack (frame { frameStacks = (ss,element:es) }:frames)

pushElements elements =
    mapM pushElement elements >> return EvalIncomplete

callStackEmpty :: Monad w => Eval a w Bool
callStackEmpty = getCallStack >>= return . null

popFrame :: Monad w => Eval a w ()
popFrame =
    do (f:cs) <- getCallStack
       stepMgr <- getStepManager
       let stepMgr' = popStepManagerFrame stepMgr
       setStepManager stepMgr'
       setCallStack cs

pushFrame name ctx line =do
    stepMgr <- getStepManager
    let stepMgr' = pushStepManagerFrame stepMgr
    setStepManager stepMgr'
    cs <- getCallStack
    setCallStack (Frame { frameName = name, frameContext = ctx, frameSourceLine = line, frameStacks = ([],[])}:cs)

getFunc :: Monad w => String -> Eval a w Func
getFunc name = incontext ("func: " ++ name) . findFunc name =<< getFuncs

setVar :: Monad w => String -> LSLValue a -> Eval a w ()
setVar name val = do
    cs <- getCallStack
    case writeVarCallStack name val cs of
        Just cs' -> setCallStack cs'
        Nothing -> setGlob =<<
            incontext ("setting " ++ name ++ ":") . writeMem name val
               =<< getGlob

getVar :: (RealFloat a, Read a, Monad w) => String -> Eval a w (LSLValue a)
getVar name =
    do cs <- getCallStack
       glob <- getGlob
       case msum [findConstVal name, readVarCallStack name cs, readMem name glob] of
           Nothing -> throwError ("no such variable " ++ name)
           Just val -> return val

initVar1 :: (RealFloat a, Read a, Monad w) => String -> LSLType -> Maybe (LSLValue a) -> Eval a w ()
initVar1 name t mval =
    do (frame:frames) <- getCallStack
       let (sc@Scope { scopeMem = m }:ss,es) = frameStacks frame
       let frame' = frame { frameStacks = (sc { scopeMem = initVar name t mval:m }:ss,es) }
       setCallStack (frame':frames)

initVars1 :: (RealFloat a, Read a, Monad w) => [Var] -> [LSLValue a] -> Eval a w ()
initVars1 vars vals =
    foldM_ (\_ -> \ (Var n t, v) -> initVar1 n t $ Just v) () $ zip vars vals

unwindToLabel name =
    let f n =
            do (frame:frames) <- getCallStack
               let (ss,es) = frameStacks frame
               case ss of
                   [] -> throwError ("label " ++ name ++ " not found")
                   (Scope {scopeLabels = l, scopeMarks = marks}:ss') ->
                       case findLBlock name l of
                           Just (LBlock _ stmts) -> return (n + marks,stmts)
                           Nothing ->
                               do setCallStack (frame { frameStacks = (ss',es) }:frames)
                                  f (n + marks + 1)
    in f 1

modMark f = do
    (frame:frames) <- getCallStack
    let (ss,es) = frameStacks frame
    case ss of
        [] -> return ()
        (sc@Scope { scopeMarks = marks }:ss') -> do
            let marks' = f marks
            let sc' = sc { scopeMarks = marks' }
            setCallStack (frame { frameStacks = (sc':ss',es) }:frames)

data ExecutionState =
      Waiting | Executing | Halted | SleepingTil Int | Erroneous String
    | Crashed String | Suspended Breakpoint | WaitingTil Int
    deriving (Show,Eq)

matchEvent :: (RealFloat t) => Event t -> [Ctx Handler] ->
    Maybe ([(String, LSLValue t)],[Ctx Statement],String,Maybe SourceContext)
matchEvent (Event name _ _) [] = Nothing
matchEvent event@(Event name values _) ((Ctx _ (Handler (Ctx ctx name') parms stmts)):hs)
        | name == name' = do mem <- bindParms (ctxItems parms) values
                             return (mem,stmts,name,ctx)
        | otherwise = matchEvent event hs

findHandler name handlers = ctx ("finding handler " ++ name) $
    findM (\ (Ctx _ (Handler (Ctx _ name') _ _)) -> name' == name) handlers


evalSimple maxTick = do
    result <- eval maxTick
    case result of
        EvalComplete Nothing ->
            do empty <- valStackEmpty
               if empty then return (result,Just VoidVal) else do
                   val <- popVal
                   return (result,Just val)
        _ -> return (result,Nothing)

setupSimple path globbindings args = do
    setScriptImageName (concat (intersperse "." path))
    updateGlobals globbindings
    (params,stmts,ctx) <- getEntryPoint path
    mem <- bindParmsForgiving params args
    initStacks
    pushFrame (concat $ intersperse "." path) ctx Nothing
    pushScope mem $ labelBlocks stmts
    pushElements [EvMark,EvBlock stmts]
    setExecutionState Executing
    where updateGlobals [] = return ()
          updateGlobals ((name,val):bs) = do
              glob' <- writeMem name val =<< getGlob
              setGlob glob'
              updateGlobals bs
          getEntryPoint [funcName] = do
              funcs <- getFuncs
              (Func (FuncDec name _ params) stmts) <- findFunc funcName funcs
              return (ctxItems params,stmts,srcCtx name)
          getEntryPoint [stateName,handlerName] = do
              (State _ handlers) <- findState stateName =<< getStates
              (Ctx _ (Handler name params stmts)) <- findHandler handlerName handlers
              return (ctxItems params,stmts, srcCtx name)

incontext s f = either throwError return f

evalScript :: (RealFloat a, Read a, Show a, Monad w) => Int -> [Event a] -> Eval a w [Event a]
evalScript maxTick queue = do
    executionState <- getExecutionState
    case executionState of
        Suspended _ -> setExecutionState Executing >> evalScript maxTick queue
        Erroneous _ -> return queue
        Waiting -> case queue of
            [] -> return queue
            (event:queue') -> do
                curState <- case event of
                    -- when a state_exit event occurs we've already changed the state variable in the image
                    -- to the new state.  the event itself contains the name of the old state, so we can
                    -- find the right event handler.
                    Event "state_exit" _ m ->
                        case M.lookup "last_state" m of
                            Just (SVal s) -> return s
                            Nothing -> logMsg "not a valid state exit!" >> getCurState
                    _ -> getCurState
                states <- getStates
                (State _ handlers) <- incontext ("state " ++ curState ++ ":") $
                    findState curState states
                case matchEvent event handlers of
                    Nothing -> return queue'
                    Just (mem,stmts,name,ctx) -> do
                        initStacks
                        pushFrame name ctx Nothing
                        pushScope mem $ labelBlocks stmts
                        pushElements [EvMark,EvBlock stmts]
                        setExecutionState Executing
                        setCurrentEvent event
                        evalScript maxTick queue'
        Executing -> do
            result <- eval maxTick
            case result of
                EvalComplete (Just newState) -> do
                    curState <- getCurState
                    setCurState newState
                    setExecutionState Waiting
                    if curState /= newState
                        -- the event queue gets cleared, with the state exit/entry events added.
                        then return [Event "state_exit" [] $ M.singleton "last_state" (SVal curState),
                                Event "state_entry" [] M.empty]
                        else return queue
                EvalComplete _ -> setExecutionState Waiting >> return queue
                YieldTil i -> setExecutionState (SleepingTil i) >> return queue
                BrokeAt bp -> setExecutionState (Suspended bp) >> return queue
                _ -> return queue
        SleepingTil i -> do
            tick <- getTick
            if (tick >= i)
               then setExecutionState Executing >> evalScript maxTick queue
               else return queue
        WaitingTil i -> do
            tick <- getTick
            if (tick >= i)
                then setExecutionState Waiting >> evalScript maxTick queue
                else return queue
        Halted -> return queue

eval :: (RealFloat a, Read a, Show a, Monad w) => Int -> Eval a w EvalResult
eval maxTick =  do
       t <- (+1) <$> getTick
       setTick t
       if t <= maxTick
           then do
               result <- eval'
               case result of
                   EvalIncomplete -> eval maxTick
                   x -> return x
           else return EvalIncomplete

eval' :: (RealFloat a, Read a, Show a, Monad w) => Eval a w EvalResult
eval' =
    let continue = return EvalIncomplete
        popAndCheck :: Monad w => Eval a w EvalResult
        popAndCheck = do
            popFrame
            noMoreFrames <- callStackEmpty
            return (if noMoreFrames then EvalComplete Nothing else EvalIncomplete)
    in do
        cs <- getCallStack
        vs <- getVStack
        es <- getEStack
        noMoreElements <- elementStackEmpty
        if noMoreElements then popAndCheck else do
            element <- popElement
            case element of
                EvReturn -> do
                   logMsg . ("return: " ++) . lslShowVal =<< peekVal
                   popAndCheck
                EvBlock [] -> popScope >> eval'
                EvMark -> modMark ((-)1) >> continue
                EvBlock (s:ss) -> pushElements [EvBlock ss,EvCtxStatement s]
                EvCtxStatement s -> do
                   pushElement (EvStatement $ ctxItem s)
                   case srcCtx s of
                       Just (SourceContext { srcTextLocation = txtl }) -> do
                           brk <- checkBp bp
                           if brk then return $ BrokeAt bp else continue
                           where bp = mkBreakpoint (textName txtl) (textLine0 txtl) 0
                       Nothing -> continue
                EvStatement (Return mexpr) -> pushElements [EvReturn,EvMexpr $ fromMCtx mexpr]
                EvStatement (NullStmt) -> eval'
                EvStatement (StateChange s) -> return $ EvalComplete $ Just s
                EvStatement (Do expr) -> pushElements [EvDiscard,EvExpr $ ctxItem expr]
                EvStatement (Decl (Var name t) Nothing) -> do
                     initVar1 name t Nothing
                     continue
                EvStatement (Decl (Var name t) mexpr) -> pushElements [EvBind name t,EvMexpr $ fromMCtx mexpr]
                EvStatement (If expr stmt1 stmt2) -> pushElements [EvCond stmt1 stmt2,EvExpr $ ctxItem expr]
                EvStatement (While expr stmt) -> modMark (+1) >> pushElements [EvMark,EvLoop (ctxItem expr) [] stmt,EvExpr $ ctxItem expr]
                EvStatement (DoWhile stmt expr) -> modMark (+1) >>
                     pushElements [EvMark,EvLoop (ctxItem expr) [] stmt, EvExpr (ctxItem expr),EvCtxStatement stmt]
                EvStatement (For mexpr1 mexpr2 mexpr3 stmt) -> do
                    let expr =  maybe (IntLit 1) ctxItem mexpr2
                    modMark (+1)
                    pushElement EvMark
                    pushElement (EvLoop expr (mexpr3) stmt)
                    pushElement (EvExpr expr)
                    pushElements [EvDiscard, EvExpr (ListExpr mexpr1)]
                    continue
                EvStatement (Compound ss) -> do
                    pushScope [] $ labelBlocks ss
                    pushElement EvMark
                    pushElement (EvBlock ss)
                    continue
                EvStatement (Label _) -> eval'
                EvStatement (Jump name) -> do
                    (n,stmts) <- unwindToLabel name
                    popMarks n
                    pushElements [EvMark,EvBlock stmts]
                    continue
                EvMexpr Nothing ->  pushVal VoidVal >> continue
                EvMexpr (Just expr) -> pushElements [EvExpr expr]
                EvDiscard -> popVal >> continue
                EvBind name t -> do
                    val <- popVal
                    initVar1 name t (Just val)
                    continue
                EvLoop expr mexpr stmt -> do
                    val <- popVal
                    when (trueCondition val) $ do
                        pushElement element               -- last, re-evaluate loop
                        pushElement (EvExpr expr)         -- next-to-last, re-evaluate expr)
                        pushElements [EvDiscard, EvExpr (ListExpr mexpr)]  -- evaluate end-of-loop expressions
                        pushElement (EvCtxStatement stmt)    -- first, evaluate statement)
                    continue
                EvCond stmt1 stmt2 -> do
                    val <- popVal
                    pushElement (EvCtxStatement (if trueCondition val then stmt1 else stmt2))
                    continue
                EvExpr (IntLit i) -> pushVal (IVal i) >> continue
                EvExpr (FloatLit f) -> pushVal (FVal (realToFrac f)) >> continue
                EvExpr (StringLit s) -> pushVal (SVal s) >> continue
                EvExpr (KeyLit k) -> pushVal (KVal $ LSLKey k) >> continue
                EvExpr (VecExpr e1 e2 e3) ->
                   -- TODO: this is probably the WRONG order of evaluation!!!
                    pushElements [EvMkVec,EvExpr $ ctxItem e3, EvExpr $ ctxItem e2, EvExpr $ ctxItem e1]
                EvExpr (RotExpr e1 e2 e3 e4) ->
                    pushElements [EvMkRot,EvExpr $ ctxItem e4, EvExpr $ ctxItem e3, EvExpr $ ctxItem e2, EvExpr $ ctxItem e1]
                EvExpr (ListExpr []) -> pushVal (LVal []) >> continue
                EvExpr (ListExpr ((Ctx _ e):es)) ->
                    pushElements [EvCons,EvExpr (ListExpr es), EvExpr e]
                EvExpr (Add expr1 expr2) -> pushBinary EvAdd expr1 expr2
                EvExpr (Sub expr1 expr2) -> pushBinary EvSub expr1 expr2
                EvExpr (Mul expr1 expr2) -> pushBinary EvMul expr1 expr2
                EvExpr (Div expr1 expr2) -> pushBinary EvDiv expr1 expr2
                EvExpr (Mod expr1 expr2) -> pushBinary EvMod expr1 expr2
                EvExpr (BAnd expr1 expr2) -> pushBinary EvBAnd expr1 expr2
                EvExpr (BOr expr1 expr2) -> pushBinary EvBOr expr1 expr2
                EvExpr (Xor expr1 expr2) -> pushBinary EvBXor expr1 expr2
                EvExpr (And expr1 expr2) -> pushBinary EvAnd expr1 expr2
                EvExpr (Or expr1 expr2) -> pushBinary EvOr expr1 expr2
                EvExpr (Lt expr1 expr2) -> pushBinary EvLt expr1 expr2
                EvExpr (Le expr1 expr2) -> pushBinary EvLe expr1 expr2
                EvExpr (Gt expr1 expr2) -> pushBinary EvGt expr1 expr2
                EvExpr (Ge expr1 expr2) -> pushBinary EvGe expr1 expr2
                EvExpr (Equal expr1 expr2) -> pushBinary EvEq expr1 expr2
                EvExpr (NotEqual expr1 expr2) -> pushBinary EvNe expr1 expr2
                EvExpr (ShiftL expr1 expr2) -> pushBinary EvShiftL expr1 expr2
                EvExpr (ShiftR expr1 expr2) -> pushBinary EvShiftR expr1 expr2
                EvExpr (Not expr) -> pushUnary EvNot expr
                EvExpr (Neg expr) -> pushUnary EvNeg expr
                EvExpr (Inv expr) -> pushUnary EvBInv expr
                EvExpr (Cast t expr) -> pushElements [EvCast t,EvExpr $ ctxItem expr]
                EvExpr (Get var) -> pushElements [EvGet $ ctxVr2Vr var]
                EvExpr (Set var expr) -> pushElements [EvSet $ ctxVr2Vr var, EvExpr $ ctxItem expr]
                EvExpr (IncBy var expr) -> pushModBy var EvAdd $ ctxItem expr
                EvExpr (DecBy var expr) -> pushModBy var EvSub $ ctxItem expr
                EvExpr (MulBy var expr) -> pushModBy var EvMul $ ctxItem expr
                EvExpr (DivBy var expr) -> pushModBy var EvDiv $ ctxItem expr
                EvExpr (ModBy var expr) -> pushModBy var EvMod $ ctxItem expr
                EvExpr (PreInc var) -> pushModBy var EvAdd (IntLit 1)
                EvExpr (PreDec var) -> pushModBy var EvSub (IntLit 1)
                EvExpr (PostInc var) -> do
                    -- after all operations, top of stack should be var value prior to increment
                    pushElement EvPop -- take that value off, leaving the original value
                    pushModBy var EvAdd (IntLit 1)
                    pushElement (EvGet $ ctxVr2Vr var) -- put the current value on the stack
                    continue
                EvExpr (PostDec var) -> do
                    -- after all operations, top of stack should be var value prior to increment
                    pushElement EvPop
                    pushModBy var EvSub (IntLit 1)
                    pushElement (EvGet $ ctxVr2Vr var) -- put the current value on the stack
                    continue
                EvExpr (Call (Ctx _ name) exprs) ->
                    case findFuncDec name predefFuncs of
                        Just (FuncDec _ t parms) -> pushElements [EvPredef name, EvExpr (ListExpr exprs)]
                        Nothing -> do
                            (Func (FuncDec ctxName t parms) stmts) <- getFunc name
                            pushElement (EvCall (ctxItem ctxName) (srcCtx ctxName) (ctxItems parms) stmts (t == LLVoid))
                            pushElement (EvExpr (ListExpr exprs))  -- first evaluate the arguments
                            continue
                EvCons -> do
                    (LVal l) <- popVal
                    val <- popVal
                    pushVal (LVal (val:l))
                    continue
                EvCall name ctx parms stmts voidFunc -> do
                    (LVal val) <- popVal -- should be the list of arguments
                    logMsg ("call: " ++ renderCall name val)
                    pushFrame name ctx Nothing
                    pushScope [] $ labelBlocks stmts
                    initVars1 parms val
                      -- a void function may not have an explicit return; if it does, this
                      -- element will get popped off without being evaluated.
                    when voidFunc (pushElement EvReturn >> pushElement (EvMexpr Nothing))
                    pushElements [EvMark,EvBlock stmts]
                    continue
                EvPredef name -> evalPredef' name
                EvGet (name,c) -> do
                    val <- getVar name
                    pushVal $ lslValueComponent c val
                    continue
                EvPop -> popVal >> continue
                EvSet (name,c) -> do
                    val <- peekVal
                    varVal <- getVar name
                    let t = typeOfLSLComponent varVal c
                    let val' = case (t,val) of
                          (LLFloat,IVal i) -> FVal (fromInt i)
                          (LLString,KVal k) -> SVal $ unLslKey k
                          (LLKey,SVal s) -> KVal $ LSLKey s
                          (t, v) | t == typeOfLSLValue v -> v
                                 | otherwise -> error ("can't implicitly convert from " ++
                                                       (show $ typeOfLSLValue v) ++
                                                       " to " ++ (show t))
                    let varVal' = replaceLslValueComponent c varVal val'
                    setVar name varVal'
                    continue
                EvAdd -> evalBinary $ \val1 val2 -> case (val1,val2) of
                    (IVal v1, IVal v2) -> IVal (v1 + v2)
                    (FVal v1, IVal v2) -> FVal (v1 + fromInt v2)
                    (IVal v1, FVal v2) -> FVal (v2 + fromInt v1)
                    (FVal v1, FVal v2) -> FVal (v1 + v2)
                    (VVal x1 y1 z1, VVal x2 y2 z2) -> VVal (x1 + x2) (y1 + y2) (z1 + z2)
                    (RVal x1 y1 z1 s1,RVal x2 y2 z2 s2) -> RVal (x1 + x2) (y1 + y2) (z1 + z2) (s1 + s2)
                    (LVal l1,LVal l2) -> LVal (l1 ++ l2)
                    (v,LVal l2) -> LVal (v:l2)
                    (LVal l1, v) -> LVal (l1 ++ [v])
                    (SVal s1,SVal s2) -> SVal (s1 ++ s2)
                    _ -> error ("invalid Add operands: " ++ (show val1) ++ ", " ++ (show val2))
                EvSub -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 - i2)
                    (IVal i1,FVal f2) -> FVal (fromInt i1 - f2)
                    (FVal f1,IVal i2) -> FVal (f1 - fromInt i2)
                    (FVal f1,FVal f2) -> FVal (f1 - f2)
                    (VVal x1 y1 z1,VVal x2 y2 z2) -> VVal (x1 - x2) (y1 - y2) (z1 - z2)
                    (RVal x1 y1 z1 s1,RVal x2 y2 z2 s2) -> RVal (x1 - x2) (y1 - y2) (z1 - z2) (s1 - s2)
                    _ -> error ("cannot apply - operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvMul -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1*i2)
                    (IVal i1,FVal f2) -> FVal (fromInt i1 * f2)
                    (FVal f1,IVal i2) -> FVal (f1 * fromInt i2)
                    (FVal f1,FVal f2) -> FVal (f1 * f2)
                    (v@(VVal _ _ _),IVal i) -> let f = fromInt i in vecMulScalar v f
                    (v@(VVal _ _ _),FVal f) -> vecMulScalar v f
                    (IVal i,v@(VVal _ _ _)) -> let f = fromInt i in vecMulScalar v f
                    (FVal f,v@(VVal _ _ _)) -> vecMulScalar v f
                    ((VVal x1 y1 z1),(VVal x2 y2 z2)) -> FVal $ x1 * x2 + y1 * y2 + z1 * z2
                    (v@(VVal _ _ _),r@(RVal _ _ _ _)) -> rotMulVec r v
                    (r1@(RVal _ _ _ _),r2@(RVal _ _ _ _)) -> rotMul r1 r2
                    _ -> error ("cannot apply * operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvDiv -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `div` i2) -- TODO: how does SL handle divide by zero?
                    (IVal i1,FVal f2) -> FVal (fromInt i1 / f2)
                    (FVal f1,IVal i2) -> FVal (f1 / fromInt i2)
                    (FVal f1,FVal f2) -> FVal (f1/f2)
                    (v@(VVal _ _ _),IVal i) -> let f = 1.0 / fromInt i in vecMulScalar v f
                    (v@(VVal _ _ _),FVal f) -> vecMulScalar v (1/f)
                    (v@(VVal _ _ _),r@(RVal _ _ _ _)) -> rotMulVec (invRot r) v
                    (r1@(RVal _ _ _ _),r2@(RVal _ _ _ _)) -> rotMul r1 $ invRot r2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvMod -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `mod` i2)
                    (v1@(VVal _ _ _),v2@(VVal _ _ _)) -> v1 `vcross` v2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvBAnd -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 .&. i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvBOr -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 .|. i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvBXor -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `xor` i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvAnd -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (if (toBool i1 && toBool i2) then 1 else 0)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvOr -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (if (toBool i1 || toBool i2) then 1 else 0)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvLt -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 < i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 < f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 < fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 < f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvLe -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 <= i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 <= f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 <= fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 <= f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvGt -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 > i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 > f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 > fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 > f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvGe -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> toLslBool $ i1 >= i2
                    (FVal f1,FVal f2) -> toLslBool $ f1 >= f2
                    (FVal f1,IVal i2) -> toLslBool $ f1 >= fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 >= f2
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvEq -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (LVal l1, LVal l2) -> toLslBool $ length l1 == length l2 -- special case of LSL weirdness
                    (SVal s, KVal k) -> toLslBool $ s == unLslKey k
                    (KVal k, SVal s) -> toLslBool $ unLslKey k == s
                    (FVal f1,IVal i2) -> toLslBool $ f1 == fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 == f2
                    (v1,v2) -> toLslBool $ v1 == v2
                EvNe -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (LVal l1, LVal l2) -> IVal (length l1 - length l2) -- special case of LSL weirdness
                    (SVal s, KVal k) -> toLslBool $ s /= unLslKey k
                    (KVal k, SVal s) -> toLslBool $ unLslKey k /= s
                    (FVal f1,IVal i2) -> toLslBool $ f1 /= fromInt i2
                    (IVal i1,FVal f2) -> toLslBool $ fromInt i1 /= f2
                    (v1,v2) -> toLslBool $ v1 /= v2
                EvShiftL -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `shiftL` i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvShiftR -> evalBinary $ \ val1 val2 -> case (val1,val2) of
                    (IVal i1,IVal i2) -> IVal (i1 `shiftR` i2)
                    _ -> error ("cannot apply operator to " ++ (show val1) ++ " and " ++ (show val2))
                EvNot -> do
                    val <- popVal
                    pushVal $ case val of
                        (IVal i) -> IVal (if i == 0 then 1 else 0)
                        _ -> error ("cannot apply operator to " ++ (show val))
                    continue
                EvBInv -> do
                    val <- popVal
                    pushVal $ case val of
                        (IVal i) -> IVal $ complement i
                        _ -> error ("cannot apply operator to " ++ (show val))
                    continue
                EvNeg -> do
                    val <- popVal
                    pushVal $ case val of
                        (IVal i) -> IVal (-i)
                        (FVal f) -> FVal (-f)
                        (VVal x y z) -> VVal (-x) (-y) (-z)
                        (RVal x y z s) -> RVal (-x) (-y) (-z) (-s)
                        _ -> error ("cannot apply operator to " ++ (show val))
                    continue
                EvCast t -> do
                    -- TODO: what are the valid typecasts?
                    -- TODO: what are the formats?
                    val <- popVal
                    pushVal $ case (t,val) of
                        (LLInteger,IVal i) -> IVal i
                        (LLInteger,FVal f) -> IVal (truncate f)
                        (LLInteger,SVal s) -> IVal (parseInt s)
                        -- TODO: can you cast a key to an int?
                        (LLFloat,FVal f) -> FVal f
                        (LLFloat,IVal i) -> FVal (fromInteger $ toInteger i)
                        (LLFloat,SVal s) -> FVal (parseFloat s)
                        -- TODO: can you cast a key to a float?
                        (LLString,v) -> toSVal v
                        -- TODO: can you cast anything but a string to a key?
                        (LLVector,SVal s) -> parseVector s
                        (LLRot,SVal s) -> parseRotation s
                        (LLKey,SVal s) -> KVal $ LSLKey s
                        (LLKey,KVal s) -> KVal s
                        (LLVector, v@(VVal _ _ _)) -> v
                        (LLRot, v@(RVal _ _ _ _)) -> v
                        (LLList, LVal l) -> LVal l
                        (LLList,v) -> LVal [v]
                    continue
                EvMkVec ->
                    revVec <$> popF <*> popF <*> popF >>= pushVal >> continue
                EvMkRot ->
                    revRot <$> popF <*> popF <*> popF <*> popF >>= pushVal
                       >> continue

revRot s z y x = RVal x y z s
revVec z y x = VVal x y z
popF = toFloat <$> popVal

trueCondition (IVal i) = (i /= 0)
trueCondition (FVal f) = (f /= 0)
trueCondition (SVal s) = not (null s)
trueCondition (LVal l) = not (null l)
trueCondition v@(VVal _ _ _) = v /= llcZeroVector
trueCondition r@(RVal _ _ _ _) = r /= llcZeroRotation
trueCondition (KVal k) = k /= nullKey &&
    case map tr $ unLslKey k of
        "ffffffff-ffff-ffff-ffff-ffffffffffff" -> True
        _ -> False
    where tr c = if c `elem` "0123456789abcdef" then 'f' else c

toLslBool bool = IVal (if bool then 1 else 0)
evalBinary f = do
    val1 <- popVal
    val2 <- popVal
    pushVal $ f val1 val2
    return EvalIncomplete

pushUnary evalElement expr = pushElements [evalElement,EvExpr $ ctxItem expr]
pushBinary evalElement expr1 expr2 = pushElements [evalElement,EvExpr $ ctxItem expr1,EvExpr $ ctxItem expr2]
pushModBy var evalElement expr = pushElements [EvSet $ ctxVr2Vr var,evalElement,EvGet $ ctxVr2Vr var,EvExpr expr]

evalPredef' name = do
    (LVal args) <- popVal
    let args' = convertArgs name args
    key <- getMyPrimKey
    sid <- getScriptName
    oid <- getObjectId
    pid <- getPrimId
    event <- getCurrentEvent
    let scriptInfo = ScriptInfo oid pid sid key event
    (evalResult,retval) <- doAction name scriptInfo args'
    pushVal retval
    return evalResult

data ExecutionInfo a = ExecutionInfo String Int (FrameInfo a) deriving (Show)

hasActiveHandler simage handler =
    case find ( \ (State ctxname _) -> curState simage == ctxItem ctxname) (states simage) of
        Nothing -> False
        Just (State _ handlers) -> isJust $ find (\ (Ctx _ (Handler ctxname _ _)) -> handler == ctxItem ctxname) handlers
