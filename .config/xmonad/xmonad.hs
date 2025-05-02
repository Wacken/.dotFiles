import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Util.EZConfig (additionalKeysP)
import Control.Monad (join, when)
import Data.Maybe (maybeToList)

-- Custom modules
import XMonad.Custom.Config
import XMonad.Custom.Keys
import XMonad.Custom.Layouts
import XMonad.Custom.Hooks

-- EWMH support for fullscreen
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

-- Main configuration
main = xmonad 
     . fullscreenSupport 
     . docks 
     . ewmh 
     . (`additionalKeysP` myKeysEz)
     $ defaults
  where
    defaults = def {
        -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask           = myModMask,
        workspaces        = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- key bindings
        keys              = myKeys,
        mouseBindings     = myMouseBindings,

        -- hooks, layouts
        manageHook = myManageHook,
        layoutHook = myLayoutHook,
        handleEventHook = myEventHook,
        logHook = myLogHook,
        startupHook = myStartupHook >> addEWMHFullscreen
    }
