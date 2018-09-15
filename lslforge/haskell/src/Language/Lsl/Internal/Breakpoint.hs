module Language.Lsl.Internal.Breakpoint(
    Breakpoint,
    BreakpointManager,
    StepManager,
    mkBreakpoint,  -- :: String -> Int -> Int -> Breakpoint
    replaceBreakpoints, -- ::  [Breakpoint] -> BreakpointManager -> BreakpointManager
    checkBreakpoint, -- :: Breakpoint -> BreakpointManager -> StepManager -> (Bool,BreakpointManager,StepManager)
    addFixedBreakpoint, -- :: Breakpoint -> BreakpointManager -> BreakpointManager
    removeFixedBreakpoint, -- :: Breakpoint -> BreakpointManager -> BreakpointManager
    pushStepManagerFrame, -- :: StepManager -> StepManager
    popStepManagerFrame, -- :: StepManager -> StepManager
    setStepOutBreakpoint, -- :: StepManager -> StepManager
    setStepBreakpoint, -- :: StepManager -> StepManager
    setStepOverBreakpoint, -- :: StepManager -> StepManager
    emptyBreakpointManager, -- :: BreakpointManager
    emptyStepManager, -- :: StepManager
    breakpointFile, -- :: Breakpoint -> String
    breakpointLine -- :: Breakpoint -> Int
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

--trace1 v = trace ("--> " ++ show v) v

data Breakpoint = Breakpoint { breakpointFile :: String, breakpointLine :: Int, breakpointColumn :: Int }
    deriving (Show,Eq,Ord)
data DynamicBreakpoint = NoDynamicBreakpoint | NextStatement | NextStatementInFrame | NextStatementAboveFrame
    deriving (Show,Eq)

newtype StepManager = StepManager [DynamicBreakpoint] deriving (Show)

mkBreakpoint name line col = Breakpoint name line col

data BreakpointManager = BreakpointManager { fixedBreakpoints :: (Map String (Set (Int,Int))) }
    deriving (Show)

emptyStepManager = StepManager []

emptyBreakpointManager = BreakpointManager M.empty

replaceBreakpoints bps bpm =
    foldl (flip addFixedBreakpoint) (bpm { fixedBreakpoints = M.empty }) bps

checkBreakpoint bp bpm sm@(StepManager dynBreakpts) =
    case dynBreakpts of
        (NextStatement:frames) -> (True,bpm, StepManager (NoDynamicBreakpoint : frames))
        (NextStatementInFrame:frames) -> (True,bpm, StepManager (NoDynamicBreakpoint : frames))
        _ -> let (result,bpm') = hasFixedBreakpoint bp bpm in (result,bpm',sm)

hasFixedBreakpoint bp bpm =
    case M.lookup (breakpointFile bp) map of
        Nothing -> (False,bpm)
        Just set -> ((breakpointLine bp, breakpointColumn bp) `S.member` set, bpm)
    where map = fixedBreakpoints bpm

addFixedBreakpoint (Breakpoint file line col) bpm =
    let map = fixedBreakpoints bpm in
        case M.lookup file map of
            Nothing -> bpm { fixedBreakpoints = M.insert file (S.singleton (line,col)) map }
            Just set -> bpm { fixedBreakpoints = M.insert file (S.insert (line,col) set) map }

removeFixedBreakpoint (Breakpoint file line col) bpm =
    let map = fixedBreakpoints bpm in
        case M.lookup file map of
            Nothing -> bpm
            Just set -> bpm { fixedBreakpoints = M.insert file (S.delete (line,col) set) map }

pushStepManagerFrame (StepManager dynBps) =
    StepManager $ case dynBps of
        (NextStatement:frames) -> (NextStatement:NoDynamicBreakpoint:frames)
        frames -> (NoDynamicBreakpoint:frames)

popStepManagerFrame (StepManager dynBps) =
   StepManager $ case dynBps of
        (NoDynamicBreakpoint:NextStatementInFrame:frames) -> NextStatement:frames
        (NoDynamicBreakpoint:x:frames) -> x:frames
        (_:_:frames) -> NextStatement:frames
        _ -> []

setStepOutBreakpoint = setDynamicBreakpoint NextStatementAboveFrame
setStepBreakpoint = setDynamicBreakpoint NextStatement
setStepOverBreakpoint = setDynamicBreakpoint NextStatementInFrame

setDynamicBreakpoint dbp (StepManager dynBps) =
    StepManager $ case  dynBps of
        (_:frames) -> dbp:frames
        [] -> []
