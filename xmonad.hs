import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Run      (spawnPipe)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.LayoutModifier
import System.Exit (exitWith, ExitCode(..))
import qualified XMonad.StackSet as W
import qualified Data.Map as M

------------------------------------------------------------------------
-- Basic settings
------------------------------------------------------------------------

myTerminal           = "kitty"
myFocusFollowsMouse  = True
myClickJustFocuses   = True
myBorderWidth        = 1
myModMask            = mod4Mask
myWorkspaces         = map show [1..9]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#3c8dff"

------------------------------------------------------------------------
-- Keybindings
------------------------------------------------------------------------

myKeys conf@(XConfig {modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- app launchers
    , ((modm,               xK_p     ), spawn "rofi -show run")
    , ((modm .|. shiftMask, xK_p     ), spawn "rofi -show drun")
    , ((modm,               xK_f     ), spawn "firefox")
    , ((0, xK_Print), spawn "scrot '%Y-%m-%d-%H-%M-%S_$wx$h_scrot.png' -e 'mv $f ~/Pictures/Screenshots/'")

    -- nemo
    , ((modm .|. shiftMask, xK_n     ), spawn "nemo --no-desktop")

    -- close window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- cycle layouts
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ layoutHook conf)

    -- refresh size
    , ((modm,               xK_n     ), refresh)

    -- move focus
    , ((modm, xK_Tab), windows W.focusDown)
    , ((modm, xK_j  ), windows W.focusDown)
    , ((modm, xK_k  ), windows W.focusUp)
    , ((modm, xK_m  ), windows W.focusMaster)

    -- swap windows
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)

    -- resize master
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)

    -- tile to float
    , ((modm, xK_t), withFocused $ \w -> do
        floats <- gets (W.floating . windowset)
        if M.member w floats
            then windows (W.sink w)  -- float → tile
            else windows (W.float w (W.RationalRect 0.25 0.25 0.5 0.5))  -- tile → float
    )

    -- Volume Controls (pulsemixer)
    , ((0, xF86XK_AudioRaiseVolume), spawn "pulsemixer --change-volume +5")
    , ((0, xF86XK_AudioLowerVolume), spawn "pulsemixer --change-volume -5")
    , ((0, xF86XK_AudioMute), spawn "pulsemixer --toggle-mute")

    -- master windows count
    , ((modm, xK_comma ), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- quit and restart
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    , ((modm,               xK_q), spawn "killall xmobar; xmonad --recompile; xmonad --restart")
    ]

    -- workspaces 1–9
    ++
    [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

    -- multi-monitor screen switching
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

------------------------------------------------------------------------
-- Mouse bindings
------------------------------------------------------------------------

myMouseBindings (XConfig {modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

------------------------------------------------------------------------
-- Layouts (now with spacing!)
------------------------------------------------------------------------

mySpacing i = spacingRaw
    True            -- enable smartBorder (ignored here)
    (Border i i i i) -- screen border
    True            -- enable screen edge gaps
    (Border i i i i) -- window border
    True            -- enable window gaps even for single window

tiled = Tall 1 (3/100) (1/2)

myLayout =
    avoidStruts $
    smartBorders (   mySpacing 6 tiled
                 ||| mySpacing 6 (Mirror tiled)
                 ||| noBorders Full
                 )

------------------------------------------------------------------------
-- ManageHook
------------------------------------------------------------------------

myManageHook = composeAll
    [ className =? "vlc"  --> doCenterFloat
    , className =? "Minecraft Linux Launcher UI"     --> doCenterFloat
    , className =? "mcpelauncher-client"     --> doCenterFloat
    , className =? "URxvt"    --> doCenterFloat
    , className =? "kitty"    --> doCenterFloat
    , className =? "Mousepad" --> doCenterFloat
    , className =? "Nemo"  --> doCenterFloat
    , className =? "Lxapearance"  --> doCenterFloat
    , className =? "Yad"  --> doCenterFloat
    , className =? "Xdg-desktop-portal-gtk"  --> doCenterFloat
    , className =? "SimpleScreenRecorder"  --> doCenterFloat
    , className =? "Xarchiver"  --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]

------------------------------------------------------------------------
-- Startup
------------------------------------------------------------------------

myStartupHook = do
    spawnOnce "compton &"
    spawnOnce "feh --bg-fill ~/Pictures/1297444.jpg &"
    spawnOnce "xrdb ~/.Xresources &"
    spawnOnce "lxpolkit &"
    spawnOnce "xset s off &"
    spawnOnce "xset s noblank &"
    spawnOnce "xset -dpms &"

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobar"
    xmonad $ ewmhFullscreen $ ewmh $ docks $ def
        { terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , clickJustFocuses   = myClickJustFocuses
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayout
        , manageHook         = myManageHook

        -- REMOVE fullscreenEventHook (deprecated)
        , handleEventHook    = handleEventHook def

        , logHook            = return ()
        , startupHook        = myStartupHook
        }

