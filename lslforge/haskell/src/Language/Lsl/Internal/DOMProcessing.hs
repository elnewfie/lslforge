{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances,
    GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.DOMProcessing(
--     Content(..),
--     Document(..),
--     Element(..),
--     Posn,
    xmlParse,
    MonadXMLAccept(..),
    unqualifiedQName,
    getContent,setContent,
    getAttrs,setAttrs,
    getTag,
    tag, tagit, clattr, clsit,
    req, opt, def, text, val,
    choice, choicec, choicet, choose,
    elist,liste,XMLAccept(..),bool,
    xmlAccept,
    runAcceptT,AcceptT(..),
    xmlAcceptT
    ) where

import Control.Monad(liftM,unless,foldM,ap)
import Control.Monad.Error(MonadError(..),ErrorT(..))
import Control.Monad.State(MonadState(..),State(..),evalState,StateT(..),
    evalStateT)
import Control.Monad.Trans
import Language.Lsl.Internal.Util(readM)
import Text.XML.HaXml(Attribute,AttValue(..),Document(..),Element(..),
    Content(..),QName(..),Reference(..),xmlParse)
import Text.XML.HaXml.Posn(Posn(..))

class MonadError String m => MonadXMLAccept m where
    getContext :: m (Element Posn)
    setContext :: Element Posn -> m ()
    withContext :: Element Posn -> m a -> m a
    withContext c a = do
        c0 <- getContext
        setContext c
        (a >>= \ v -> setContext c0 >> return v)
            `catchError`
            ( \ e -> setContext c0 >> throwError e)

unqualifiedQName :: QName -> String
unqualifiedQName (N n) = n
unqualifiedQName (QN _ n) = n

setContent :: MonadXMLAccept m => [Content Posn] -> m ()
setContent c = getContext >>= \ (Elem nm attr _) -> setContext (Elem nm attr c)

setAttrs :: MonadXMLAccept m => [Attribute] -> m ()
setAttrs a = getContext >>= \ (Elem nm _ c) -> setContext (Elem nm a c)

getContent :: MonadXMLAccept m => m [Content Posn]
getContent = getContext >>= \ (Elem _ _ c) -> return c

getTag :: MonadXMLAccept m => m String
getTag = getContext >>= \ (Elem tag _ _) -> return (unqualifiedQName tag)

getAttrs :: MonadXMLAccept m => m [Attribute]
getAttrs = getContext >>= \ (Elem _ attrs _) -> return attrs

type CAcceptor m t = m t

runAcc :: MonadXMLAccept m => m b -> (a -> m b) -> CAcceptor m (Maybe a) -> m b
runAcc nf f acceptor = do
    content <- getContent
    find [] content `catchError`
        \ s -> getTag >>= \ t -> throwError ("in element " ++ t ++ ": " ++ s)
    where find bs [] = nf
          find bs (ce@(CElem e@(Elem nm attrs ec) i):cs) =
              (try (unqualifiedQName nm) e i >>= maybe (find (ce:bs) cs) (\ v -> setContent (reverse bs ++ cs) >> f v))
          find bs (ce:cs) = find (ce:bs) cs
          try nm e i = withContext e acceptor `catchError` \ s ->
              throwError ("in element " ++ nm ++ " in " ++ show i ++ ": " ++ s)

tag :: MonadXMLAccept m => String -> CAcceptor m ()
tag t = do
    tagMatches <- (==) <$> pure t <*> getTag
    unless tagMatches $ throwError ("tag " ++ t ++ " not matched")

tagit :: MonadXMLAccept m => String -> CAcceptor m a
    -> CAcceptor m (Maybe a)
tagit s acc = do
    found <- (tag s >> return True) `catchError` const (return False)
    if found then Just <$> acc else return Nothing

clattr :: MonadXMLAccept m => String -> CAcceptor m ()
clattr c = do
    attrs <- getAttrs
    unless ((N "class",AttValue [Left c]) `elem` attrs) $
        throwError ("attribute class=\"" ++ c ++ "\" not matched")

clsit :: MonadXMLAccept m => String -> CAcceptor m a
    -> CAcceptor m (Maybe a)
clsit s acc =
    (clattr s >> Just <$> acc) `catchError` const (return Nothing)

-- req : this element is a required child, with the specified tag
req t acceptor = runAcc (throwError ("element not found: " ++ t)) return
    (tagit t acceptor)

opt t acceptor = runAcc (return Nothing) (return . Just)
    (tagit t acceptor)
def t def acceptor = runAcc (return def) return (tagit t acceptor)

text :: (MonadXMLAccept m) => m String
text = do
    s <- (liftM concat . mapM textItem) =<< getContent
    setContent []
    return s

val :: (MonadXMLAccept m, Read a) => CAcceptor m a
val = text >>= readM

bool = text >>= \ s -> case s of
    "true" -> return True
    "false" -> return False
    "True" -> return True
    "False" -> return False
    s -> throwError ("unacceptable bool value: " ++ s)

textItem :: MonadXMLAccept m => Content Posn -> m String
textItem (CElem _ i) = throwError ("unexpected element in " ++ show i)
textItem (CString _ s _) = return s
textItem (CRef (RefEntity "lt") _) = return "<"
textItem (CRef (RefEntity "gt") _) = return ">"
textItem (CRef (RefEntity "amp") _) = return "&"
textItem (CRef (RefEntity "quot") _) = return "\""
textItem (CRef (RefEntity "apos") _) = return "'"
textItem (CMisc _ i) = throwError ("unexpected content in " ++ show i)

choose :: MonadXMLAccept m => CAcceptor m a -> CAcceptor m a -> CAcceptor m a
choose i j = i `catchError` (const j)

choice :: MonadXMLAccept m => [CAcceptor m (Maybe a)] -> CAcceptor m (Maybe a)
choice [] = return Nothing --throwError "nothing matched"
choice (c:cs) = c >>= maybe (choice cs) (return . Just)

choicec :: MonadXMLAccept m => [(String,CAcceptor m a)] -> CAcceptor m (Maybe a)
choicec = choice . map (uncurry clsit)

choicet :: MonadXMLAccept m => [(String,CAcceptor m a)] -> CAcceptor m (Maybe a)
choicet = choice . map (uncurry tagit)

elist :: MonadXMLAccept m => CAcceptor m (Maybe a) -> CAcceptor m [a]
elist acc = foldM f [] =<< getContent where
    f l (CElem e@(Elem nm _ _) i) = --withContext e acc >>= return . (l ++) . (:[])
        withContext e acc >>=
            maybe (throwError ("unexpected element " ++ show nm ++ " in list in " ++ show i))
            (return . (l ++) . (:[]))
    f l (CMisc _ i) = err i
    f l (CRef _ i) = err i
    f l (CString _ _ i) = return l
    err i = throwError ("in " ++ show i ++ " unexpected non-element content")

liste s = (elist . tagit s)

newtype AcceptT m a = AcceptT { unAcceptT :: ErrorT String (StateT (Element Posn) m) a }
   deriving (Monad)

instance MonadTrans AcceptT where
    lift v = AcceptT { unAcceptT = lift $ lift v }

instance Monad m => MonadState (Element Posn) (AcceptT m) where
    get = AcceptT { unAcceptT = get }
    put v = AcceptT { unAcceptT = put v }

instance Monad m => MonadError String (AcceptT m) where
    throwError e = AcceptT { unAcceptT = throwError e }
    catchError v f = AcceptT { unAcceptT = catchError (unAcceptT v) (unAcceptT . f) }

instance Monad m => Functor (AcceptT m) where
    fmap = liftM

instance Monad m => Applicative (AcceptT m) where
    pure = return
    (<*>) = ap

instance Monad m => MonadXMLAccept (AcceptT m) where
    getContext = get
    setContext = put

runAcceptT = (evalStateT . runErrorT . unAcceptT)
xmlAcceptT c s = runAcceptT c root where
    (Document _ _ root _) = xmlParse "input" s

newtype XMLAccept a = XMLAccept { unXMLAccept :: ErrorT String (State (Element Posn)) a }
    deriving (Monad)

instance MonadState (Element Posn) XMLAccept where
   get = XMLAccept { unXMLAccept = get }
   put v = XMLAccept { unXMLAccept = put v }

instance MonadError String XMLAccept where
    throwError e = XMLAccept { unXMLAccept = throwError e }
    catchError v f = XMLAccept { unXMLAccept = catchError (unXMLAccept v) (unXMLAccept . f) }

instance Functor XMLAccept where
    fmap = liftM

instance Applicative XMLAccept where
   pure  = return
   (<*>) = ap

instance MonadXMLAccept XMLAccept where
   getContext = get
   setContext = put

runXMLAccept = (evalState . runErrorT . unXMLAccept)
xmlAccept c s = runXMLAccept c root where
    (Document _ _ root _) = xmlParse "input" s
