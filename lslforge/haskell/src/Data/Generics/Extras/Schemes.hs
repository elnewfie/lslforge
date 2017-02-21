{-# LANGUAGE Rank2Types #-}
module Data.Generics.Extras.Schemes (
    downup,
    downupSkipping,
    everythingTwice,
    everythingButTwice,
    everywhereButM
 ) where

------------------------------------------------------------------------------

import Data.Data
import Data.Generics.Aliases

everythingButTwice :: GenericQ Bool -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r -> GenericQ r
everythingButTwice q k def f g x | q x = def
                                 | otherwise = (foldl k (f x) (gmapQ (everythingButTwice q k def f g) x) `k` (g x))

everythingTwice :: (r -> r -> r) -> GenericQ r -> GenericQ r -> GenericQ r
everythingTwice k f g x
  = foldl k (f x) (gmapQ (everythingTwice k f g) x) `k` (g x)


downup :: Monad m => GenericM m -> GenericM m -> GenericM m

downup down up x = do x' <- down x
                      x'' <- gmapM (downup down up) x'
                      up x''

downupSkipping :: Monad m => GenericQ Bool -> GenericM m -> GenericM m -> GenericM m
downupSkipping skip down up x | skip x = return x
                              | otherwise = do x' <- down x
                                               x'' <- gmapM (downupSkipping skip down up) x'
                                               up x''

-- | Monadic variation on everywhere
everywhereButM :: Monad m => GenericQ Bool -> GenericM m -> GenericM m

-- Bottom-up order is also reflected in order of do-actions
everywhereButM p f x | p x = return x
                     | otherwise = do x' <- gmapM (everywhereButM p f) x
                                      f x'
