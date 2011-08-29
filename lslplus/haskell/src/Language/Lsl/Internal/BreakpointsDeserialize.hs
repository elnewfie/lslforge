{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Internal.BreakpointsDeserialize(bp, bps, module Language.Lsl.Internal.Breakpoint) where

import Control.Applicative
import Language.Lsl.Internal.DOMProcessing(req,text,val,liste)
import Language.Lsl.Internal.Breakpoint(mkBreakpoint)

bp = mkBreakpoint <$> req "file" text <*> req "line" val <*> pure 0
bps = liste "breakpoint" bp