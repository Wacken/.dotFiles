module XMonad.Custom.Keys (myKeys, myKeysEz, myMouseBindings) where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Layout.Gaps (Direction2D(..), GapMessage(..), setGaps)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Custom commands
clipboardy :: MonadIO m => m ()
clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"

-- Eww dashboard commands
centerlaunch = spawn "exec ~/.local/bin/eww open-many blur_full weather profile quote search_full disturb-icon vpn-icon home_dir screenshot power_full reboot_full lock_full logout_full suspend_full"
sidebarlaunch = spawn "exec ~/.local/bin/eww open-many weather_side time_side smol_calendar player_side sys_side sliders_side"
ewwclose = spawn "exec ~/.local/bin/eww close-all"

-- Screenshot commands
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"
maimsave = spawn "maim -s ~/Desktop/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"

-- Application launcher
rofi_launcher = spawn "rofi -show combi -modes combi -combi-modes \"window,drun,run\" -no-lazy-grab -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "

-- Main keybindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- System Controls
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_F1    ), spawn "betterlockscreen -l")
    , ((modm .|. shiftMask, xK_q     ), spawn "~/.local/bin/powermenu.sh")
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Launchers
    , ((modm,               xK_p     ), rofi_launcher)
    , ((modm,               xK_o     ), centerlaunch)
    , ((modm .|. shiftMask, xK_o     ), ewwclose)
    , ((modm,               xK_s     ), sidebarlaunch)
    , ((modm .|. shiftMask, xK_s     ), ewwclose)

    -- Media Controls
    , ((0, xF86XK_AudioPlay       ), spawn "playerctl -p youtube-music play-pause")
    , ((0, xF86XK_AudioPrev       ), spawn "playerctl -p youtube-music previous")
    , ((0, xF86XK_AudioNext       ), spawn "playerctl -p youtube-music next")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
    , ((0, xF86XK_AudioMute       ), spawn "pactl set-sink-mute 0 toggle")

    -- Brightness Controls
    , ((0, xF86XK_MonBrightnessUp  ), spawn "brightnessctl s +10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
 
    -- Screenshot
    , ((0,                    xK_Print), maimcopy)
    , ((modm,                 xK_Print), maimsave)

    -- Custom Scripts
    , ((modm,               xK_b     ), spawn "exec ~/.local/bin/bartoggle")
    , ((modm,               xK_z     ), spawn "exec ~/.local/bin/inhibit_activate")
    , ((modm .|. shiftMask, xK_z     ), spawn "exec ~/.local/bin/inhibit_deactivate")
    , ((modm .|. shiftMask, xK_b     ), clipboardy)
    , ((modm,               xK_d     ), spawn "exec ~/.local/bin/do_not_disturb.sh")

    -- Window Management
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Gaps Management
    , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)
    , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps [(L,30), (R,30), (U,40), (D,60)])
    , ((modm .|. controlMask, xK_t), sendMessage $ IncGap 10 L)
    , ((modm .|. shiftMask, xK_t     ), sendMessage $ DecGap 10 L)
    , ((modm .|. controlMask, xK_y), sendMessage $ IncGap 10 U)
    , ((modm .|. shiftMask, xK_y     ), sendMessage $ DecGap 10 U)
    , ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D)
    , ((modm .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)
    , ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R)
    , ((modm .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)

    -- Window Copying
    , ((modm, xK_a), windows copyToAll)
    , ((modm .|. shiftMask, xK_a), killAllOtherCopies)
    ]
    ++
    -- Workspace Switching
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- Screen Switching
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Emacs keybindings
myKeysEz :: [(String, X ())]
myKeysEz =
  [ ("M4-e e", spawn "emacsclient -c -a ''")
  , ("M4-e d", spawn "emacsclient -c -a '' --eval '(dired nil)'")
  , ("M4-e v", spawn "emacsclient -c -a '' --eval '(vterm \"monad\")'")
  , ("M4-e n", spawn "emacsclient -c -a '' --eval '(elfeed)'")
  , ("M4-e t", spawn "emacsclient -c -a '' --eval '(org-capture)'")
  , ("M4-e g", spawn "emacsclient -c -a '' --eval '(org-agenda nil \"G\")'")
  , ("M4-l", spawn "rofi-pass")
  ]

-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ] 