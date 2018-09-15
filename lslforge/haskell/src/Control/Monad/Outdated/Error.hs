-- We encountered problem that LSLForge process hung up when start up.
-- I knew it was caused by difference of 'fail' for Either monad behavier
-- between base version prior 4.3 and later.
-- Prior 4.3, fail returns Left e, but later throws exception.
-- Language.Lsl.Internal.Util.lookupM is called in 3 monads,
-- Maybe, Either and WorldE, using to search various things.
-- In a case to search module name from library list given by Eclipse,
-- lookupM calls fail when module name not found,
-- it's ok on GHC 6.10.4 because it same to Left,
-- but GHC using base 4.3 or later (including 8.0.2) throws exception
-- and cause LSLForge process to error-exit.
-- I copied Error class from GHC library
-- to reproduce old 'fail' method. By Pell Smit

module Control.Monad.Outdated.Error (
    Error(..),
    ErrorList(..)
  ) where

import Control.Monad.Fail(MonadFail(..))

-- This class was copied from Control.Monad.Trans.Error
-- which will be removed from standard library.
-- But we still need this class for fail on Either monad.
class Error e where
    noMsg  :: e
    noMsg    = strMsg ""
    strMsg :: String -> e
    strMsg _ = noMsg

instance ErrorList a => Error [a] where
    strMsg = listMsg

class ErrorList a where
    listMsg :: String -> [a]

instance ErrorList Char where
    listMsg = id

-- This is core to reproduce old Either fail.
instance Error e => MonadFail (Either e) where
    fail s = Left $ strMsg s
