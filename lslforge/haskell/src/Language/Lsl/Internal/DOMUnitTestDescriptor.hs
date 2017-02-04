{-# LANGUAGE FlexibleContexts,NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.DOMUnitTestDescriptor(tests) where

import Control.Applicative
import Control.Monad.Error(MonadError(..))
import Data.List(isSuffixOf)
import Language.Lsl.Internal.DOMProcessing(liste,req,text,opt,choicec,choicet,
    elist)
import Language.Lsl.Internal.ExpressionHandler(evaluateExpression)
import Language.Lsl.Internal.Type(LSLType(..),LSLValue(..))
import Language.Lsl.UnitTest(LSLUnitTest(..),EntryPoint(..),FuncCallExpectations(..),ExpectationMode(..))

tests = liste "lsl-test" testE

testE = LSLUnitTest <$> req "name" text <*> req "entryPoint" entryPointE
    <*> req "initialBindings" bindingsE <*> req "arguments" argsE
    <*> req "expectations" expectationsE <*> req "expectedReturn" maybeV
    <*> req "finalBindings" bindingsE <*> pure Nothing

entryPointE = do
    path <- req "path" text
    fileName <- req "fileName" text
    ep fileName path
    where ep nm path
            | "lslm" `isSuffixOf` nm = return $ ModuleFunc nm path
            | otherwise = case parts path of
             [a,b] -> return $ ScriptHandler nm a b
             [a] -> return $ ScriptFunc nm a
             _ -> throwError ("invalid function/handler path: " ++ path)

argsE = elist $ choicet lslValTypes

expectationsE = FuncCallExpectations <$> req "mode" (text >>= decodeMode) <*> req "calls" callsE

decodeMode "nice" = return Nice
decodeMode "exhaust" = return Exhaust
decodeMode "strict" = return Strict
decodeMode "normal" = return Normal
decodeMode s = throwError ("illegal mode " ++ s)

callsE = filter (not . null . fst . fst) <$> liste "call" callE

callE = ( \ x y z -> ((x,y),z)) <$> req "name" text <*> req "args" callArgsE
    <*> req "returns" someV

callArgsE = liste "maybe-value" maybeV

bindingsE = liste "globalBinding" globalBindingE
globalBindingE = (,) <$> req "name" text <*> req "value" someV

evalE typ = evaluateExpression typ <$> text >>= maybe
    (throwError ("invalid content for " ++ show typ))
    return

lslValTypes = [
    ("lsl-string",evalE LLString),
    ("lsl-key",evalE LLKey),
    ("lsl-integer",evalE LLInteger),
    ("lsl-float",evalE LLFloat),
    ("lsl-vector",evalE LLVector),
    ("lsl-rotation",evalE LLRot),
    ("lsl-list1",evalE LLList),
    ("lsl-void",return VoidVal)]

maybeV = opt "val" someV
someV =  choicec lslValTypes >>=
    maybe (throwError "unrecognized lsl value type") return

parts :: String -> [String]
parts "" =  []
parts s  =  let (l, s') = break (== '.') s
            in  l : case s' of
                      []      -> []
                      (_:s'') -> parts s''
