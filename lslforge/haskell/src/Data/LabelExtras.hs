{-# LANGUAGE TemplateHaskell, TypeOperators, NoMonomorphismRestriction #-}
module Data.LabelExtras(
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
    mkLabelsAlt,
    lfst,
    lsnd,
    lfst3,
    lsnd3,
    l3rd3
    ) where

import Prelude hiding(id,(.),lookup)
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import qualified Control.Monad.State as SM
import Control.Monad.State(MonadState)
import Data.Map hiding(filter,map)
import qualified Data.IntMap as IM
import Data.Label
import Language.Haskell.TH

liftML :: (Applicative m, Monad m) => a :-> b -> m a :-> m b
liftML l = lens getter modifier where
    getter = fmap (get l)
    setter = liftA2 (set l)
    modifier = isoL getter setter where
        isoL g s m f = s (m (g f)) f

-- | a lens for a Map element
lm :: (Applicative m, MonadError e m, Error e, Show k, Ord k) => k -> m (Map k v) :-> m v
lm k = lens getter modifier where
    getter mm = mm >>= (maybe (throwError $ err k)  return) . lookup k
    modifier f mm = do
        v <- f $ getter mm
        insert k v <$> mm
    err k = strMsg $ "key " ++ show k ++ " not found"

lmi :: (Applicative m, MonadError e m, Error e) => Int -> m (IM.IntMap v) :-> m v
lmi i = lens getter modifier where
    getter mm = mm >>= (maybe (throwError $ err i)  return) . IM.lookup i
    modifier f mm = do
        v <- f $ getter mm
        IM.insert i v <$> mm
    err i = strMsg $ "key " ++ show i ++ " not found"

lli :: (Applicative m, MonadError e m, Error e) => Int -> m [v] :-> m v
lli i = lens getter modifier where
    getter ml = ml >>= (maybe (throwError $ err i)  return) . safeIndex i
    modifier f ml = do
        v <- f $ getter ml
        ml >>= \l -> maybe (throwError $ err i) return $ replace i l v
    err i = strMsg $ "index " ++ show i ++ " not found"

safeIndex i l | i >= 0 && i < length l = Just (l !! i)
              | otherwise = Nothing
replace i l v
    | i >= 0 && i < length l = let (xs,y:ys) = splitAt i l in Just (xs ++ (v:ys))
    | otherwise = Nothing

getM :: MonadState s m => m s :-> m b -> m b
getM l = get l SM.get

setM :: MonadState s m => m s :-> m b -> b -> m ()
setM l v = set l (return v) SM.get >>= SM.put

modM_ :: MonadState s m => m s :-> m b -> (b -> b) -> m ()
modM_ l f = modify l (liftM f) SM.get >>= SM.put

modM :: MonadState s m => m s :-> m b -> (b -> b) -> m b
modM l f = do
    v <- getM l
    l =: f v
    return v

infixr 7 =:
(=:) :: MonadState s m => m s :-> m b -> b -> m ()
(=:) = setM

mkLabelsAlt :: [Name] -> Q [Dec]
mkLabelsAlt names = do
    decs <- filter isFuncDec <$> mkLabels names
    decs2 <- mapM liftDec decs
    return (map change decs ++ decs2)
    where liftDec (FunD nm _) = funD (mkName (nameBase nm))
              [clause [] (normalB $ (appE (varE 'liftML) (varE (mkName $ nameBase nm ++ "U")))) []]
          change (FunD nm x) = FunD (mkName (nameBase nm ++ "U")) x
          isFuncDec (FunD _ _) = True
          isFuncDec _          = False

lfstU :: (a,b) :-> a
lfstU = lens fst (\f (x,y) -> (f x,y))

lsndU :: (a,b) :-> b
lsndU = lens snd (\f (x,y) -> (x,f y))

lfst3U :: (a,b,c) :-> a
lfst3U = lens (\(x,_,_) -> x) (\f (x,y,z) -> (f x,y,z))

lsnd3U :: (a,b,c) :-> b
lsnd3U = lens (\(_,y,_) -> y) (\f (x,y,z) -> (x,f y,z))

l3rd3U :: (a,b,c) :-> c
l3rd3U = lens (\(_,_,z) -> z) (\f (x,y,z) -> (x,y,f z))

lfst :: (Applicative m, Monad m) => m (a,b) :-> m a
lfst = liftML $ lfstU

lsnd :: (Applicative m, Monad m) => m (a,b) :-> m b
lsnd = liftML $ lsndU

lfst3 :: (Applicative m, Monad m) => m (a,b,c) :-> m a
lfst3 = liftML $ lfst3U

lsnd3 :: (Applicative m, Monad m) => m (a,b,c) :-> m b
lsnd3 = liftML $ lsnd3U

l3rd3 :: (Applicative m, Monad m) => m (a,b,c) :-> m c
l3rd3 = liftML $ l3rd3U

getI :: Identity a :-> Identity b -> a -> b
getI l = runIdentity . get l . return

setI :: Identity a :-> Identity b -> b -> a -> a
setI l b a = runIdentity $ set l (return b) (return a)

modI :: Identity a :-> Identity b -> (b -> b) -> a -> a
modI l f v = runIdentity $ modify l (liftM f) (return v)

infixr 6 :*

data (:*) a b = a :* b

infixr 6 .*

(.*) :: Monad m => m a :-> m b -> m a :-> m c -> m a :-> m (b :* c)
(.*) l1 l2 = lens getter modifier where
    getter ma = (:*) `liftM` get l1 ma `ap` get l2 ma
    modifier f ma = f (getter ma) >>=
                    \(b:*c) -> set l1 (return b) . set l2 (return c) $ ma

-- so we can say v .: foo.bar.baz
(.:) = flip getI

(.?) :: (Applicative m, MonadError e m, Error e)
     => m b :-> m c -> m a :-> m b -> m a :-> m (Maybe c)
(.?) l1 l2 = lens getter modifier where
    getter ma = do
        b <- get l2 ma
        (Just `liftM` get l1 (return b)) `catchError` const (return Nothing)
    modifier g ma = do
        b <- get l2 ma
        mc <- g (Just <$> (get l1 . get l2 $ ma))
        maybe ma (f b) mc
        where f b c = do
                  b <- (Just `liftM` (set l1 (return c) (return b)))
                       `catchError` const (return Nothing)
                  maybe ma (flip (set l2) ma . return) b

infixr 8 .?
infixr 8 .:

rjoinV :: (Applicative m, MonadError e m, Error e)
       => e -> m a :-> m (Maybe b) -> m a :-> m b
rjoinV e l = lens getter modifier where
    getter ma = do
        maybeb <- get l ma
        maybe (throwError e) return maybeb
    modifier f ma = do
        let b = Just <$> f (getter ma)
        set l b ma

rjoin :: (Applicative m, MonadError e m, Error e)
      => m a :-> m (Maybe b) -> m a :-> m b
rjoin = rjoinV (strMsg "failed")
