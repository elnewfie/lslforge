{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.ExpressionHandler(validateExpression,evaluateExpression) where

import Control.Monad.Except
import Data.Bits
import System.IO

import Language.Lsl.Internal.Constants
import Language.Lsl.Internal.DOMProcessing(req,tag,text,xmlAccept)
import Language.Lsl.Parse
import Language.Lsl.Syntax
import Language.Lsl.Internal.Key(LSLKey(..))
import Language.Lsl.Internal.Type
import Language.Lsl.Internal.Util
import Language.Lsl.Internal.XmlCreate hiding (emit)
import qualified Language.Lsl.Internal.XmlCreate as E

emit s = E.emit s []

validPrimitiveCtxExpr (Ctx _ expr) = validPrimitiveExpr expr

validPrimitiveExpr :: Expr -> Either String LSLType
validPrimitiveExpr (Get (Ctx _ name,All)) = maybe (fail "not found!") Right $ findConstType name
validPrimitiveExpr (Neg expr) =
    do t <- validPrimitiveCtxExpr expr
       when (t `notElem` [LLFloat,LLInteger,LLVector,LLRot]) $ fail "operator only valid for integer, float, vector, and rotation expressions"
       return t
validPrimitiveExpr (Inv expr) =
    do t <- validPrimitiveCtxExpr expr
       when (t /= LLInteger) $ fail "operator only valid for integer expressions"
       return LLInteger
validPrimitiveExpr (Not expr) =
    do t <- validPrimitiveCtxExpr expr
       when (t /= LLInteger) $ fail "operator only valid for integer expressions"
       return LLInteger
validPrimitiveExpr (IntLit i) = return LLInteger
validPrimitiveExpr (FloatLit f) = return LLFloat
validPrimitiveExpr (StringLit s) = return LLString
validPrimitiveExpr (KeyLit k) = return LLKey
validPrimitiveExpr (VecExpr x y z) = do
     t0 <- validPrimitiveCtxExpr x
     t1 <- validPrimitiveCtxExpr y
     t2 <- validPrimitiveCtxExpr z
     when (not (all (`elem` [LLInteger,LLFloat]) [t0,t1,t2])) $ fail "vector expression must contain only integer or float components"
     return LLVector
validPrimitiveExpr (RotExpr x y z s) = do
     t0 <- validPrimitiveCtxExpr x
     t1 <- validPrimitiveCtxExpr y
     t2 <- validPrimitiveCtxExpr z
     t3 <- validPrimitiveCtxExpr s
     when (not (all (`elem` [LLInteger,LLFloat]) [t0,t1,t2,t3])) $ fail "vector expression must contain only integer or float components"
     return LLRot
validPrimitiveExpr (ListExpr l) = do
    ts <- mapM validPrimitiveCtxExpr l
    when (LLList `elem` ts) $ fail "lists can't contain lists"
    return LLList
validPrimitiveExpr (Add e0 e1) =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLFloat
           (LLFloat,LLInteger) -> return LLFloat
           (LLFloat,LLFloat) -> return LLFloat
           (LLInteger,LLInteger) -> return LLInteger
           (LLVector,LLVector) -> return LLVector
           (LLRot,LLRot) -> return LLVector
           (LLString,LLString) -> return LLString
           _ -> fail "incompatible operands"
validPrimitiveExpr (Sub e0 e1) =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLFloat
           (LLInteger,LLInteger) -> return LLInteger
           (LLFloat,LLInteger) -> return LLFloat
           (LLFloat,LLFloat) -> return LLFloat
           (LLVector,LLVector) -> return LLVector
           (LLRot,LLRot) -> return LLRot
           _ -> fail "incompatible operands"
validPrimitiveExpr (Mul e0 e1) =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLFloat
           (LLFloat,LLFloat) -> return LLFloat
           (LLInteger,LLInteger) -> return LLInteger
           (LLFloat,LLInteger) -> return LLFloat
           (LLFloat,LLVector) -> return LLVector
           (LLVector,LLFloat) -> return LLVector
           (LLInteger,LLVector) -> return LLVector
           (LLVector,LLInteger) -> return LLVector
           (LLVector,LLRot) -> return LLVector
           (LLRot,LLRot) -> return LLRot
           _ -> fail "incompatible operands"
validPrimitiveExpr (Div e0 e1) =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLInteger) -> return LLInteger
           (LLInteger,LLFloat) -> return LLFloat
           (LLFloat,LLInteger) -> return LLFloat
           (LLFloat,LLFloat) -> return LLFloat
           (LLVector,LLInteger) -> return LLVector
           (LLVector,LLFloat) -> return LLVector
           (LLVector,LLRot) -> return LLVector
           (LLRot,LLRot) -> return LLRot
           _ -> fail "incompatible operands"
validPrimitiveExpr (Mod e0 e1) =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLInteger) -> return LLInteger
           (LLVector,LLVector) -> return LLVector
           _ -> fail "incompatible operands"
validPrimitiveExpr (BAnd e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (BOr e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (Xor e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (ShiftL e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (ShiftR e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (And e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (Or e0 e1) = validIntegerExpr e0 e1
validPrimitiveExpr (Equal e0 e1) = validEqExpr e0 e1
validPrimitiveExpr (NotEqual e0 e1) = validEqExpr e0 e1
validPrimitiveExpr (Lt e0 e1) = validRelExpr e0 e1
validPrimitiveExpr (Le e0 e1) = validRelExpr e0 e1
validPrimitiveExpr (Gt e0 e1) = validRelExpr e0 e1
validPrimitiveExpr (Ge e0 e1) = validRelExpr e0 e1
validPrimitiveExpr (Cast t e) = do
    t' <- validPrimitiveCtxExpr e
    when (not $ isCastValid t' t) $ fail "invalid cast"
    return t
validPrimitiveExpr expr = fail "expression not valid in this context"

validPrimEach e0 e1 =
    do t0 <- validPrimitiveCtxExpr e0
       t1 <- validPrimitiveCtxExpr e1
       return (t0,t1)

validIntegerExpr e0 e1 =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLInteger) -> return LLInteger
           _ -> fail "incompatible operands"

validRelExpr e0 e1 =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLInteger
           (LLFloat,LLInteger) -> return LLInteger
           (LLFloat,LLFloat) -> return LLInteger
           (LLInteger,LLInteger) -> return LLInteger
           _ -> fail "incompatible operands"

validEqExpr e0 e1 =
    do (t0,t1) <- validPrimEach e0 e1
       case (t0,t1) of
           (LLInteger,LLFloat) -> return LLInteger
           (LLInteger,LLInteger) -> return LLInteger
           (LLFloat,LLFloat) -> return LLInteger
           (LLFloat,LLInteger) -> return LLInteger
           (LLString,LLString) -> return LLInteger
           (LLKey,LLKey) -> return LLInteger
           _ -> fail "incompatible operands"

checkExpr t text =
   case exprParser text of
       Left _ -> fail "syntax error"
       Right expr ->
           case validPrimitiveCtxExpr expr of
               Right t' ->
                   case (t,t') of
                       (LLString,LLKey) -> return expr
                       (LLKey,LLString) -> return expr
                       (LLFloat,LLInteger) -> return expr
                       (t,t') | t == t' -> return expr
                              | otherwise -> fail (lslTypeString t ++ " expected")
               Left s -> fail s

evaluateExpression t text =
    case checkExpr t text of
        Left s -> fail s
        Right expr ->
            do v <- evalCtxExpr expr
               case (t,v) of
                   (LLKey,SVal s) -> return $ KVal (LSLKey s)
                   (LLString,KVal k) -> return $ SVal $ unLslKey k
                   (LLFloat,IVal i) -> return $ FVal (fromInt i)
                   _ -> return v

unexpectedValue :: Monad m => m a
unexpectedValue = fail "unexpected value"

evalCtxExpr (Ctx _ expr) = evalExpr expr

evalExpr (Get (Ctx _ name,All)) = findConstVal name
evalExpr (Neg expr) =
    do v <- evalCtxExpr expr
       case v of
           (IVal i) -> return (IVal (-i))
           (FVal f) -> return (FVal (-f))
           _ -> unexpectedValue
evalExpr (Inv expr) =
    do t <- evalCtxExpr expr
       case t of
           (IVal i) -> return (IVal $ complement i)
           _ -> unexpectedValue
evalExpr (Not expr) =
    do t <- evalCtxExpr expr
       case t of
           (IVal i) -> return $ IVal (if i == 0 then 1 else 0)
evalExpr (IntLit i) = return (IVal i)
evalExpr (FloatLit f) = return (FVal $ realToFrac f)
evalExpr (StringLit s) = return (SVal s)
evalExpr (KeyLit k) = return (KVal $ LSLKey k)
evalExpr (VecExpr xExpr yExpr zExpr) =
    do x <- evalCtxExpr xExpr
       y <- evalCtxExpr yExpr
       z <- evalCtxExpr zExpr
       return (VVal (toFloat x) (toFloat y) (toFloat z))
evalExpr (RotExpr xExpr yExpr zExpr sExpr) =
    do x <- evalCtxExpr xExpr
       y <- evalCtxExpr yExpr
       z <- evalCtxExpr zExpr
       s <- evalCtxExpr sExpr
       return (RVal (toFloat x) (toFloat y) (toFloat z) (toFloat s))
evalExpr (ListExpr l) =
    do vals <- mapM evalCtxExpr l
       return (LVal vals)
evalExpr (Add e0 e1) =
    do (v0,v1) <- evalEach e0 e1
       case (v0,v1) of
           (IVal i,FVal f) -> return $ FVal (f + fromInt i)
           (FVal f,IVal i) -> return $ FVal (f + fromInt i)
           (FVal f0,FVal f1) -> return $ FVal (f0 + f1)
           (IVal i0,IVal i1) -> return $ IVal (i0 + i1)
           (SVal s0,SVal s1) -> return $ SVal (s0 ++ s1)
           (VVal x y z,VVal x' y' z') -> return $ VVal (x + x') (y + y') (z + z')
           (RVal x1 y1 z1 s1,RVal x2 y2 z2 s2) -> return $ RVal (x1 + x2) (y1 + y2) (z1 + z2) (s1 + s2)
           _ -> fail "incompatible operands"
evalExpr (Sub e0 e1) =
    do (v0,v1) <- evalEach e0 e1
       case (v0,v1) of
           (IVal i1,IVal i2) -> return $ IVal (i1 - i2)
           (IVal i1,FVal f2) -> return $ FVal (fromInt i1 - f2)
           (FVal f1,IVal i2) -> return $ FVal (f1 - fromInt i2)
           (FVal f1,FVal f2) -> return $ FVal (f1 - f2)
           (VVal x1 y1 z1,VVal x2 y2 z2) -> return $ VVal (x1 - x2) (y1 - y2) (z1 - z2)
           (RVal x1 y1 z1 s1,RVal x2 y2 z2 s2) -> return $ RVal (x1 - x2) (y1 - y2) (z1 - z2) (s1 - s2)
           _ -> fail "incompatible operands"
evalExpr (Mul e0 e1) =
    do (v0,v1) <- evalEach e0 e1
       case (v0,v1) of
            (IVal i1,IVal i2) -> return $ IVal (i1*i2)
            (IVal i1,FVal f2) -> return $ FVal (fromInt i1 * f2)
            (FVal f1,IVal i2) -> return $ FVal (f1 * fromInt i2)
            (FVal f1,FVal f2) -> return $ FVal (f1 * f2)
            (v@(VVal _ _ _),IVal i) -> let f = fromInt i in return $ vecMulScalar v f
            (v@(VVal _ _ _),FVal f) -> return $ vecMulScalar v f
            ((VVal x1 y1 z1),(VVal x2 y2 z2)) -> return $ FVal $ x1 * x2 + y1 * y2 + z1 * z2
            (v@(VVal _ _ _),r@(RVal _ _ _ _)) -> return $ rotMulVec r v
            (r1@(RVal _ _ _ _),r2@(RVal _ _ _ _)) -> return $ rotMul r1 r2
            _ -> fail "incompatible operands"
evalExpr (Div e0 e1) =
    do (v0,v1) <- evalEach e0 e1
       case (v0,v1) of
            (IVal i1,IVal i2) -> return $ IVal (i1 `div` i2) -- TODO: how does SL handle divide by zero?
            (IVal i1,FVal f2) -> return $ FVal (fromInt i1 / f2)
            (FVal f1,IVal i2) -> return $ FVal (f1 / fromInt i2)
            (FVal f1,FVal f2) -> return $ FVal (f1/f2)
            (v@(VVal _ _ _),IVal i) -> let f = 1.0 / fromInt i in return $ vecMulScalar v f
            (v@(VVal _ _ _),FVal f) -> return $ vecMulScalar v f
            (v@(VVal _ _ _),r@(RVal _ _ _ _)) -> return $ rotMulVec (invRot r) v
            (r1@(RVal _ _ _ _),r2@(RVal _ _ _ _)) -> return $ rotMul r1 $ invRot r2
            _ -> fail "incompatible operands"
evalExpr (Mod e0 e1) =
    do (v0,v1) <- evalEach e0 e1
       case (v0,v1) of
           (IVal i1,IVal i2) -> return $ IVal (i1 `mod` i2)
           (v1@(VVal _ _ _),v2@(VVal _ _ _)) ->return $ v1 `vcross` v2
           _ -> fail "incompatible operands"
evalExpr (BAnd e0 e1) = evalIntExpr (.&.) e0 e1
evalExpr (BOr e0 e1) = evalIntExpr (.|.) e0 e1
evalExpr (Xor e0 e1) = evalIntExpr xor e0 e1
evalExpr (ShiftL e0 e1) = evalIntExpr shiftL e0 e1
evalExpr (ShiftR e0 e1) = evalIntExpr shiftR e0 e1
evalExpr (And e0 e1) = evalIntExpr (\ x y -> if x /= 0 && y /= 0 then 1 else 0) e0 e1
evalExpr (Or e0 e1) = evalIntExpr (\ x y -> if x /= 0 || y /= 0 then 1 else 0) e0 e1
evalExpr (Equal e0 e1) = evalEqExpr (==) e0 e1
evalExpr (NotEqual e0 e1) = evalEqExpr (/=) e0 e1
evalExpr (Lt e0 e1) = evalRelExpr (<) (<) e0 e1
evalExpr (Le e0 e1) = evalRelExpr (<=) (<=) e0 e1
evalExpr (Gt e0 e1) = evalRelExpr (>) (>) e0 e1
evalExpr (Ge e0 e1) = evalRelExpr (>=) (>=) e0 e1
evalExpr (Cast t e) = do
    v <- evalCtxExpr e
    case (t,v) of
       (LLInteger,IVal i) -> return $ IVal i
       (LLInteger,FVal f) -> return $ IVal (truncate f)
       (LLInteger,SVal s) -> return $ IVal (parseInt s)
       -- TODO: can you cast a key to an int?
       (LLFloat,FVal f) -> return $ FVal f
       (LLFloat,IVal i) -> return $ FVal (fromInteger $ toInteger i)
       (LLFloat,SVal s) -> return $ FVal (parseFloat s)
       -- TODO: can you cast a key to a float?
       (LLString,v) -> return $ toSVal v
       -- TODO: can you cast anything but a string to a key?
       (LLVector,SVal s) -> return $ parseVector s
       (LLRot,SVal s) -> return $ parseRotation s
       (LLList,SVal s) -> return $ LVal [SVal s]
       (LLKey,SVal s) -> return $ KVal $ LSLKey s
       (LLKey,KVal s) -> return $ KVal s
       (LLVector, (VVal _ _ _)) -> return v
       (LLRot, (RVal _ _ _ _)) -> return v
       (LLList, LVal l) -> return v
       _ -> fail "invalid cast!"

evalExpr expr = fail "expression not valid in this context"

evalEqExpr f e0 e1 =
    do (v0,v1) <- evalEach e0 e1
       let b = case (v0,v1) of
               (FVal f0, IVal i1) -> f (FVal f0) (FVal $ fromInt i1)
               (IVal i0, FVal f1) -> f (FVal $ fromInt i0) (FVal f1)
               (SVal s, KVal k) -> f (SVal s) (SVal $ unLslKey k)
               (KVal k, SVal s) -> f (SVal s) (SVal $ unLslKey k)
               (x,y) -> f x y
       return $ IVal $ if b then 1 else 0

evalRelExpr fi ff e0 e1 =
    do (v0,v1) <- evalEach e0 e1
       let b = case (v0,v1) of
               (FVal f, IVal i) -> ff f (fromInt i)
               (IVal i, FVal f) -> ff (fromInt i) f
               (FVal f0, FVal f1) -> ff f0 f1
               (IVal i0, IVal i1) -> fi i0 i1
       return $ IVal $ if b then 1 else 0

evalIntExpr f e0 e1 =
    do (IVal i0, IVal i1) <- evalEach e0 e1
       return $ IVal $ f i0 i1

evalEach e0 e1 =
    do v0 <- evalCtxExpr e0
       v1 <- evalCtxExpr e1
       return (v0,v1)

extractExpressionFromXML s = either error id $ xmlAccept expression s

expression = tag "expression" >> (,) <$> req "type" text <*> req "text" text

expressionValidatorEmitter (Left s) =
    (emit "result" [ emit "ok" [showString "false"], emit "msg" [showString $ xmlEscape s]]) ""
expressionValidatorEmitter (Right _) =
    (emit "result" [ emit "ok" [showString "true"]]) ""

validateExpression hIn hOut =
    do input <- hGetContents hIn
       let (tstring,text) = extractExpressionFromXML input
       let t = case parseType tstring of
                   Right v -> v
                   Left s -> error (show s)
       putStr $ expressionValidatorEmitter (checkExpr t text)
