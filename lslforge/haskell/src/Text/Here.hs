{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Here
-- Copyright   :  (c) Rob Greayer 2009
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  robgreayer@yahoo.com
-- Stability   :  provisional
-- Portability :  non-portable (uses existentially template haskell)
--
-- An implementation of 'here' documents, using quasiquotation.
-- 
-----------------------------------------------------------------------------
module Text.Here(here) where

import Data.Generics.Aliases(extQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(..),dataToPatQ,dataToExpQ)

filt [] = []
filt ('\r':'\n':cs) = '\n':filt cs
filt (c:cs) = c:filt cs

herePat :: String -> TH.Q TH.Pat
herePat s = dataToPatQ (const Nothing) (filt s)
hereExp :: String -> TH.Q TH.Exp
hereExp s = dataToExpQ (const Nothing) (filt s)

-- | A quasi-quoter for a string...
here :: QuasiQuoter
here = QuasiQuoter { quoteExp = hereExp, quotePat = herePat }
