{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Language.Lsl.Internal.Pragmas(Pragma(..)) where

import Data.Data(Data,Typeable)

data Pragma = PragmaInline | PragmaNoInline
    deriving (Show,Ord,Eq,Typeable,Data)