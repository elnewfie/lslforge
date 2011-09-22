{-# OPTIONS_GHC -XTemplateHaskell #-}
module Language.Lsl.Internal.AccessGenerator(genAccessorsForType,genMAccessorsForType) where

import Language.Haskell.TH
import Control.Monad.State

genAccessors (ConT nm) = genAccessorsForType nm
genAccessors _ = fail "can't generate accessors for specified type"

genAccessorsForType nm = do
    info <- reify nm
    case info of
        TyConI d -> generateAccessorsForDec d
        _ -> fail $ "can't generate accessors for specified name: " ++ show nm

generateAccessorsForDec (DataD _ _ _ [con] _) = generateAccessorsForCon con
generateAccessorsForDec (DataD _ _ _ _ _) = fail "can't generate accessors for data type with multiple constructors"
generateAccessorsForDec _ = fail "can only generate accessors for 'data' types (not even newtypes)"

generateAccessorsForCon (RecC _ vs) = generateAccessorsForSelectors vs
generateAccessorsForCon _ = fail "can only generate accessors for constructors with record selectors (and not quantified constructors)"

generateAccessorsForSelectors vs = mapM generateAccessorForSelector vs
generateAccessorForSelector (nm,_,_) = 
    let nm' = mkName ("set'" ++ nameBase nm) in do
        nm0 <- newName "x"
        nm1 <- newName "y"
        return (FunD nm' [Clause [VarP nm0, VarP nm1] (NormalB (RecUpdE (VarE nm0) [(nm,VarE nm1)])) []])

genMAccessors (ConT nm) = genAccessorsForType nm
genMAccessors _ = fail "can't generate accessors for specified type"

genMAccessorsForType nm = do
    info <- reify nm
    case info of
        TyConI d -> generateMAccessorsForDec d
        _ -> fail $ "can't generate accessors for specified name: " ++ show nm

generateMAccessorsForDec (DataD _ _ _ [con] _) = generateMAccessorsForCon con
generateMAccessorsForDec (DataD _ _ _ _ _) = fail "can't generate accessors for data type with multiple constructors"
generateMAccessorsForDec _ = fail "can only generate accessors for 'data' types (not even newtypes)"

generateMAccessorsForCon (RecC _ vs) = generateMAccessorsForSelectors vs
generateMAccessorsForCon _ = fail "can only generate accessors for constructors with record selectors (and not quantified constructors)"

generateMAccessorsForSelectors vs = 
    mapM generateMAccessorForSelector vs >>= return . concat
    
generateMAccessorForSelector (nm,_,_) = 
    let nmPut' = mkName ("put'" ++ nameBase nm) 
        nmGet' = mkName ("get'" ++ nameBase nm)
        nmCompose = '(.)
        nmBind = '(>>= )
        nmGet = 'get
        nmPut = 'put
        nmReturn = 'return
    in do
        nm0 <- newName "x"
        nm1 <- newName "s"
        return [(FunD nmGet' [Clause [] (NormalB (AppE (AppE (VarE nmBind) (VarE nmGet)) (AppE (AppE (VarE nmCompose) (VarE nmReturn)) (VarE nm)))) []]),
                (FunD nmPut' [Clause [VarP nm0] 
                    (NormalB 
                        -- get >>= \ s -> put (s { nm = nm0 })
                        (AppE (AppE (VarE nmBind) (VarE nmGet)) (LamE [VarP nm1] (AppE (VarE nmPut) (RecUpdE (VarE nm1) [(nm,VarE nm0)]))))
                    ) []])]
             