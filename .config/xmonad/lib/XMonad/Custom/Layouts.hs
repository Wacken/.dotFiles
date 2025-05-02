module XMonad.Custom.Layouts (myLayoutHook) where

import XMonad
import XMonad.Layout.Spacing (spacingRaw, Border(Border))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Gaps (gaps, Direction2D(..))
import XMonad.Hooks.ManageDocks (avoidStruts)

-- Base layouts
baseLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- Layout hook with modifications
myLayoutHook = gaps [(L,30), (R,30), (U,40), (D,60)] 
             $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True 
             $ smartBorders baseLayout 