module Language.Lsl.UnitTest(
        LSLUnitTest(..),
        FuncCallExpectations(..),
        ExpectationMode(..),
        EntryPoint(..),
        Binding,
        expectedReturns,
        removeExpectation) where

import Control.Monad(liftM2)
import Data.List(maximumBy)
import Language.Lsl.Internal.Type(LSLValue(..))
import Language.Lsl.Internal.Exec(Binding(..))
import Language.Lsl.Internal.Util(removeLookup)

data FuncCallExpectations a = FuncCallExpectations {
    expectationMode :: ExpectationMode,
    callList :: [((String, [Maybe (LSLValue a)]),LSLValue a)] } deriving (Show)

data ExpectationMode = Nice | Normal | Exhaust | Strict deriving (Show,Eq)

data EntryPoint = ModuleFunc String String | ScriptFunc String String | ScriptHandler String String String
    deriving (Show)

data LSLUnitTest = LSLUnitTest {
        unitTestName :: String,
        entryPoint :: EntryPoint,
        initialGlobs :: [Binding Float],
        arguments :: [LSLValue Float],
        expectedCalls :: FuncCallExpectations Float,
        expectedReturn :: Maybe (LSLValue Float),
        expectedGlobalVals :: [Binding Float],
        expectedNewState :: Maybe String
    } deriving (Show)

argMatch (Just x) y | lslValuesMatch x y = Just 1
                    | otherwise          = Nothing
argMatch Nothing _                       = Just 0
argsMatch expectArgs args = foldl (liftM2 (+)) (Just 0) $ zipWith argMatch expectArgs args

lslValuesMatch (FVal a) (FVal b) = a == b ||
    if a == 0.0 then b < 0.000001
    else if b == 0.0 then a < 0.000001
    else abs ((b - a) / a) <  0.000001
lslValuesMatch x y = x == y

matchFail :: Monad m => m a
matchFail = fail "no matching call"

expectedReturns :: (RealFloat a,Monad m) => String -> [LSLValue a] -> FuncCallExpectations a -> m ((String,[Maybe (LSLValue a)]),LSLValue a)
expectedReturns name args (FuncCallExpectations Strict (match@((name',expectArgs),returns):_)) =
    if name /= name' || argsMatch expectArgs args == Nothing then matchFail
    else return match
expectedReturns name args (FuncCallExpectations Strict _) = matchFail
expectedReturns n a (FuncCallExpectations mode callList) =
    let rightNames = filter ((n==) . fst . fst) callList
        argMatch (Just x) y | x == y    = Just 1
                            | otherwise = Nothing
        argMatch Nothing _              = Just 0
        argsMatch args expectArgs = foldl (liftM2 (+)) (Just 0) $ zipWith argMatch expectArgs args
        ranked = zip (map (argsMatch a) (map (snd . fst) rightNames)) rightNames
        orderMatch (Nothing,_) (Nothing,_) = EQ
        orderMatch _ (Nothing,_) = GT
        orderMatch (Nothing,_) _ = LT
        orderMatch ((Just a),_) ((Just b),_) = compare a b
    in case ranked of
        [] -> fail matchFail
        _ -> case maximumBy orderMatch ranked of
            (Nothing,_) -> matchFail
            (_,e) -> return e

removeExpectation :: (RealFloat a) => (String,[Maybe (LSLValue a)]) -> FuncCallExpectations a -> FuncCallExpectations a
removeExpectation m fce = fce { callList = removeLookup m (callList fce) }
