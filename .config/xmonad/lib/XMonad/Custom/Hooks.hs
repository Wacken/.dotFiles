module XMonad.Custom.Hooks (myManageHook, myEventHook, myLogHook, myStartupHook) where

import XMonad
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Layout.Fullscreen (fullscreenManageHook)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Window rules
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , title     =? "Picture-in-picture" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat
    ]

-- Event handling
myEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "kitty") (return True)

-- Status bars and logging
myLogHook :: X ()
myLogHook = return ()

-- Startup hook
myStartupHook = do
    -- Bar and UI
    spawnOnce "exec ~/.local/bin/bartoggle"
    spawnOnce "exec ~/.local/bin/eww daemon"
    spawn "xsetroot -cursor_name left_ptr"
    
    -- Background and compositor
    spawnOnce "feh --bg-scale /home/wacken/wallpapers/yosemite-lowpoly.jpg"
    spawnOnce "picom --experimental-backends"
    
    -- System utilities
    spawnOnce "greenclip daemon"
    spawnOnce "dunst"
    spawnOnce "redshift -t 6500:3000"
    spawnOnce "udiskie &"
    spawnOnce "ibus-daemon -drx &"
    
    -- Applications
    spawnOnce "/usr/bin/emacs --daemon &" 