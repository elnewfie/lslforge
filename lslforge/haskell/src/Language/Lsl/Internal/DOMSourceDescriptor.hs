{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.DOMSourceDescriptor(sources) where

import Language.Lsl.Internal.DOMProcessing

item = (,) <$> req "identifier" text <*> req "path" text

itemlist = liste "item" item
sources = (,,) <$> def "optimize" False bool
    <*> req "modules" itemlist <*> req "scripts" itemlist
