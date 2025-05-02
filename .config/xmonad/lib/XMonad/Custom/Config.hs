module XMonad.Custom.Config 
    ( myTerminal
    , myFocusFollowsMouse
    , myClickJustFocuses
    , myBorderWidth
    , myModMask
    , myWorkspaces
    , myNormalBorderColor
    , myFocusedBorderColor
    ) where

import XMonad

-- Terminal
myTerminal = "kitty"

-- Whether focus follows the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels
myBorderWidth :: Dimension
myBorderWidth = 2

-- ModMask (mod4Mask = super key)
myModMask = mod4Mask

-- Workspaces
-- Icons require a compatible font (e.g., Font Awesome)
myWorkspaces = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]

-- Border colors
myNormalBorderColor = "#3b4252"  -- Nord theme color
myFocusedBorderColor = "#bc96da" -- Custom purple 