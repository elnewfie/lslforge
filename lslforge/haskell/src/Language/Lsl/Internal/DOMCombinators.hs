{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}
module Language.Lsl.Internal.DOMCombinators where

import Control.Monad.State
import Control.Monad.Error

import Data.Maybe
import Language.Lsl.Internal.DOMProcessing
import Text.XML.HaXml(Attribute,AttValue(..),Document(..),Element(..),Content(..),Reference(..),xmlParse,info)
import Text.XML.HaXml.Posn(Posn(..),noPos)
import Language.Lsl.Internal.Util(readM)
import Debug.Trace

type ContentAcceptor a = [Content Posn] -> Either String a
type ContentFinder a = StateT [Content Posn] (Either String) a
type ElementAcceptor a = Posn -> Element Posn -> Either String a
type ElementTester a = Posn -> Element Posn -> Either String (Maybe a)
type AttributeAcceptor a = Posn -> [Attribute] -> Either String a
type AttributeFinder a = StateT (Posn,[Attribute]) (Either String) a
type AttributeTester a = Posn -> Attribute -> Either String (Maybe a)
type AttributesTester a = Posn -> [Attribute] -> Either String (Maybe a)

el :: String -> (b -> a) -> ContentAcceptor b -> ElementTester a
el tag f cf p (Elem name _ cs) | tag /= unqualifiedQName name = Right Nothing
                               | otherwise = case cf cs of
                                        Left s -> Left ("at " ++ show p ++ ": " ++ s)
                                        Right v -> Right (Just (f v))

elWith :: String -> (a -> b -> c) -> AttributeAcceptor (Maybe a) -> ContentAcceptor b -> ElementTester c
elWith tag f af cf p (Elem name attrs cs) | tag /= unqualifiedQName name = Right Nothing
                                          | otherwise = do
                                                   av <- af p attrs
                                                   case av of
                                                       Nothing -> Right Nothing
                                                       Just av -> do
                                                           cv <- cf cs
                                                           return (Just (f av cv))

liftElemTester :: (Posn -> (Element Posn) -> Either String (Maybe a)) -> (Content Posn -> Either String (Maybe a))
liftElemTester ef (CElem e pos) = case ef pos e of
    Left s -> Left ("at " ++ show pos ++ ": " ++ s)
    Right v -> Right v

canHaveElem :: ElementTester a -> ContentFinder (Maybe a)
canHaveElem ef = get >>= \ cs ->
        mapM (\ c -> (lift . liftElemTester ef) c >>= return . (,) c) [ e | e@(CElem _ _) <- cs ]
        >>= (\ vs -> case span (isNothing . snd) vs of
    (bs,[]) -> put (map fst bs) >> return Nothing
    (bs,c:cs) -> put (map fst (bs ++ cs)) >> return (snd c))

mustHaveElem :: ElementTester a -> ContentFinder a
mustHaveElem ef = get >>= \ cs ->
        mapM (\ c -> (lift . liftElemTester ef) c >>= return . (,) c) [ e | e@(CElem _ _) <- cs ]
        >>= (\ vs -> case span (isNothing . snd) vs of
    (bs,[]) -> throwError ("element not found")
    (bs,c:cs) -> put (map fst (bs ++ cs)) >> return (fromJust $ snd c))

mustHave :: String -> ContentAcceptor a -> ContentFinder a
mustHave s ca = catchError (mustHaveElem (el s id ca)) (\ e -> throwError (e ++ " (" ++ s ++ ")"))

canHave :: String -> ContentAcceptor a -> ContentFinder (Maybe a)
canHave s ca = canHaveElem (el s id ca)

comprises :: ContentFinder a -> ContentAcceptor a
comprises cf cs = case runStateT cf cs of
    Left s -> throwError s
    Right (v,cs') -> empty cs' >> return v

many :: ElementTester a -> ContentAcceptor [a]
many et cs = case runStateT go cs of
        Left s -> throwError ("many: " ++ s)
        Right (v,cs') -> empty cs' >> return v
    where go = do
            isEmpty <- get >>= return . null
            if isEmpty then return []
                       else do
                           v <- mustHaveElem et
                           vs <- go
                           return (v:vs)

attContent :: AttValue -> String
attContent (AttValue xs) = foldl (flip (flip (++) . either id refToString)) [] xs

refToString (RefEntity s) = refEntityString s
refToString (RefChar i) = [toEnum i]

attrIs :: String -> String -> AttributeTester ()
attrIs k v _ (nm,attv) | v == attContent attv  && k == unqualifiedQName nm = return (Just ())
                       | otherwise = return Nothing

hasAttr :: AttributeTester a -> AttributeFinder (Maybe a)
hasAttr at = get >>= \ (pos,attrs) -> mapM (lift . at pos) attrs >>= return . zip attrs >>= (\ ps -> case span (isNothing . snd) ps of
    (bs,[]) -> return Nothing
    (bs,c:cs) -> put (pos,map fst (bs ++ cs)) >> return (snd c))

thisAttr :: String -> String -> AttributesTester ()
thisAttr k v p atts = case runStateT (hasAttr (attrIs k v)) (p,atts) of
    Left s -> throwError ("at " ++ show p ++ ": " ++ s)
    Right (Nothing,(_,l)) -> return Nothing
    Right (v,(_,[]))       -> return v
    _ ->  throwError ("at " ++ show p ++ ": unexpected attributes")

infixr 1 <|>

(<|>) :: ElementTester a -> ElementTester a -> ElementTester a
(<|>) l r p e = case l p e of
     Left s -> throwError ("at: " ++ show p ++ s)
     Right Nothing -> r p e
     Right v -> return v

nope :: ElementTester a
nope _ _ = return Nothing

choice :: [ElementTester a] -> ElementTester a
choice = foldl (<|>) nope

boolContent cs = simpleContent cs >>= (\ v -> case v of
    "true" -> Right True
    "false" -> Right False
    s -> Left ("unrecognized bool " ++ s))

readableContent :: Read a => ContentAcceptor a
readableContent cs = simpleContent cs >>= readM

refEntityString "lt" = "<"
refEntityString "gt" = ">"
refEntityString "amp" = "&"
refEntityString "quot" = "\""
refEntityString "apos" = "'"
refEntityString _ = "?"

simpleContent :: ContentAcceptor String
simpleContent cs = mapM processContentItem cs >>= return . concat
    where
        processContentItem (CElem (Elem name _ _) _) = Left ("unexpected content element (" ++ show name ++ ")")
        processContentItem (CString _ s _) = Right s
        processContentItem (CRef (RefEntity s) _) = Right $ refEntityString s
        processContentItem (CRef (RefChar i) _) = Right $ [toEnum i]
        processContentItem (CMisc _ _) = Right "unexpected content"

empty :: ContentAcceptor ()
empty [] = Right ()
empty (c:_) = Left ("unexpected content at" ++ show (info c))
----------------------



data Foo = Bar { x :: Int, y :: String, z :: Maybe Double }
         | Baz { q :: String, r :: Int }
    deriving Show

bar :: ContentAcceptor Foo
bar = comprises $ do
        x <- mustHave "x" readableContent
        y <- mustHave "y" simpleContent
        z <- canHave "z" readableContent
        return (Bar x y z)
baz = comprises $ do
    q <- mustHave "q" simpleContent
    r <- mustHave "r" readableContent
    return (Baz q r)

fooE = el "BarFoo" id bar
   <|> el "BazFoo" id baz

fooAs :: String -> ElementTester Foo
fooAs s = elWith s (const id) (thisAttr "class" "BarFoo") bar
      <|> elWith s (const id) (thisAttr "class" "BazFoo") baz

data Zzz = Zzz { content :: [Foo], bleah :: Foo } deriving Show

zzzE = el "Zzz" id $ comprises (mustHave "content" (many fooE) >>= \ cs -> mustHaveElem (fooAs "bleah") >>= \ b -> return $ Zzz cs b)

parse :: ElementAcceptor a -> String -> Either String a
parse eaf s = eaf noPos el
    where Document _ _ el _ = xmlParse "" s
