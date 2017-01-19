{-# LANGUAGE TemplateHaskell, TypeOperators, NoMonomorphismRestriction #-}
module Data.Record.LabelExtras(
    lm,
    lmi,
    lli,
    liftML,
    getM,
    setM,
    modM,
    modM_,
    getI,
    setI,
    modI,
    (=:),
    (:*)(..),
    (.*),
    (.?),
    (.:),
    rjoin,
    rjoinV,
    mkLabelsPlus,
    mkLabelsAlt,
    lfst,
    lsnd,
    lfst3,
    lsnd3,
    l3rd3
    ) where

import Prelude hiding(id,(.),lookup,mod)
import Control.Category
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Error.Class
import qualified Control.Monad.State as SM
import Control.Monad.State(MonadState)
import Data.Map hiding(map)
import qualified Data.IntMap as IM
import Data.Record.Label hiding(getM,setM,modM,(=:))
import Language.Haskell.TH

liftML :: Monad m => a :-> b -> m a :-> m b
liftML l = lens (liftM $ getL l) (liftM2 $ setL l)

-- | a lens for a Map element
lm :: (MonadError e m, Error e, Show k, Ord k) => k -> m (Map k v) :-> m v
lm k = lens getter setter where
    getter mm = mm >>= (maybe (throwError $ err k)  return) . lookup k
    setter mv mm = mv >>= \ v -> insert k v `liftM` mm
    err k = strMsg $ "key " ++ show k ++ " not found"

lmi :: (MonadError e m, Error e) => Int -> m (IM.IntMap v) :-> m v
lmi i = lens getter setter where
    getter mm = mm >>= (maybe (throwError $ err i)  return) . IM.lookup i
    setter mv mm = mv >>= \ v -> IM.insert i v `liftM` mm
    err i = strMsg $ "key " ++ show i ++ " not found"

lli :: (MonadError e m, Error e) => Int -> m [v] :-> m v
lli i = lens getter setter where
    getter ml = ml >>= (maybe (throwError $ err i)  return) . safeIndex i
    setter mv ml = do
        v <- mv
        l <- ml
        maybe (throwError $ err i) return $ replace i l v
    err i = strMsg $ "index " ++ show i ++ " not found"
    
safeIndex i l | i >= 0 && i < length l = Just (l !! i)
              | otherwise = Nothing
replace i l v 
    | i >= 0 && i < length l = let (xs,y:ys) = splitAt i l in Just (xs ++ (v:ys))
    | otherwise = Nothing
    
getM :: MonadState s m => m s :-> m b -> m b
getM l = getL l SM.get

setM :: MonadState s m => m s :-> m b -> b -> m ()
setM l v = setL l (return v) SM.get >>= SM.put

modM_ :: MonadState s m => m s :-> m b -> (b -> b) -> m ()
modM_ l f = modL l (liftM f) SM.get >>= SM.put

modM :: MonadState s m => m s :-> m b -> (b -> b) -> m b
modM l f = do
    v <- getM l
    l =: f v
    return v
    
infixr 7 =:
(=:) :: MonadState s m => m s :-> m b -> b -> m ()
(=:) = setM

mkLabelsPlus :: [Name] -> Q [Dec]
mkLabelsPlus names = do
    decs <- mkLabelsNoTypes names
    decs2 <- mapM liftDec decs
    return (decs ++ decs2)
    where liftDec (FunD nm _) = funD (mkName (nameBase nm ++ "M"))
              [clause [] (normalB $ (appE (varE 'liftML) (varE nm))) []]

mkLabelsAlt :: [Name] -> Q [Dec]
mkLabelsAlt names = do
    decs <- mkLabelsNoTypes names
    decs2 <- mapM liftDec decs
    return (map change decs ++ decs2)
    where liftDec (FunD nm _) = funD (mkName (nameBase nm))
              [clause [] (normalB $ (appE (varE 'liftML) (varE (mkName $ nameBase nm ++ "U")))) []]
          change (FunD nm x) = FunD (mkName (nameBase nm ++ "U")) x
          
lfstU :: (a,b) :-> a
lfstU = lens fst (\ x (_,y) -> (x,y))

lsndU :: (a,b) :-> b
lsndU = lens snd (\ y (x,_) -> (x,y))

lfst3U :: (a,b,c) :-> a
lfst3U = lens (\ (x,_,_) -> x) (\ x (_,y,z) -> (x,y,z))

lsnd3U :: (a,b,c) :-> b
lsnd3U = lens (\ (_,y,_) -> y) (\ y (x,_,z) -> (x,y,z))

l3rd3U :: (a,b,c) :-> c
l3rd3U = lens (\ (_,_,z) -> z) (\ z (x,y,_) -> (x,y,z))

lfst :: Monad m => m (a,b) :-> m a
lfst = liftML $ lfstU

lsnd :: Monad m => m (a,b) :-> m b
lsnd = liftML $ lsndU

lfst3 :: Monad m => m (a,b,c) :-> m a
lfst3 = liftML $ lfst3U

lsnd3 :: Monad m => m (a,b,c) :-> m b
lsnd3 = liftML $ lsnd3U

l3rd3 :: Monad m => m (a,b,c) :-> m c
l3rd3 = liftML $ l3rd3U

getI :: Identity a :-> Identity b -> a -> b
getI l = runIdentity . getL l . return

setI :: Identity a :-> Identity b -> b -> a -> a
setI l b a = runIdentity $ setL l (return b) (return a)

modI :: Identity a :-> Identity b -> (b -> b) -> a -> a
modI l f v = runIdentity $ modL l (liftM f) (return v)

infixr 6 :*

data (:*) a b = a :* b

infixr 6 .*

(.*) :: Monad m => m a :-> m b -> m a :-> m c -> m a :-> m (b :* c)
(.*) l1 l2 = lens getter setter where
    getter ma = (:*) `liftM` getL l1 ma `ap` getL l2 ma
    setter mv a = mv >>= \ (b:*c) -> setL l1 (return b) . setL l2 (return c) $ a
    
-- so we can say v .: foo.bar.baz
(.:) = flip getI

(.?) :: (MonadError e m, Error e) => m b :-> m c -> m a :-> m b -> m a :-> m (Maybe c)
(.?) l1 l2 = lens getter setter where
    getter ma = do
        b <- getL l2 ma
        (Just `liftM` getL l1 (return b)) `catchError` const (return Nothing)
    setter mmc ma = do
        b <- getL l2 ma
        mc <- mmc
        maybe ma (f b) mc
        where f b c = do
                  b <- (Just `liftM` (setL l1 (return c) (return b)))
                      `catchError` const (return Nothing)
                  maybe ma (flip (setL l2) ma . return) b
    
infixr 8 .?
infixr 8 .:

rjoinV :: (MonadError e m, Error e) => e -> m a :-> m (Maybe b) -> m a :-> m b
rjoinV e l = lens getter setter where
    getter ma = do
        maybeb <- getL l ma
        maybe (throwError e) return maybeb
    setter mb ma = do
        b <- Just `liftM` mb
        setL l (return b) ma
        
rjoin = rjoinV (strMsg "failed")
