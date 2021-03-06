-- My XMonad config file

import XMonad
import Data.Monoid
import System.Exit
-- import XMonad.Actions.CycleRecentWS -- I don't understand how to configure this
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SwapWorkspaces
import XMonad.Layout.ShowWName
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Minimize

import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "lxterminal"
-- xterm -u8 -ls -fg lightgreen -bg black"
--  -fg white -bg black"
-- -fa 'Misc' -fs 10"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myPrompt :: XPConfig
myPrompt = defaultXPConfig {
             position = Top
           , font = "xft:Consolas-14"
           , height = 24
           -- Zenburn!:
           , bgColor = "#3F3F3F"
           , fgColor = "#EFEFEF"
           , fgHLight = "#000D18"
           , bgHLight = "#8FAF9F"
           , borderColor = "#719E7F"
           }

-- Width of the window border in pixels.
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask
-- myModMask = mod4Mask
-- the obvious choice is windows key (window manager)! plus alt is not ergonomic on my kbd.

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["0", "1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#aaaaaa"
myFocusedBorderColor = "#0000ff"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- ?
    -- , ((modm,               xK_p     ), spawn "dmenu_run")
    -- , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- , ((modm, 0x1008ff03), spawn "sudo brightness -")
    -- , ((modm, 0x1008ff02), spawn "sudo brightness +")

    , ((modm, xK_F1), spawn "light -U 10")
    , ((modm, xK_F2), spawn "light -A 10")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
      
 --   , ((modm,               xK_m     ), withFocused minimizeWindow)
 --   , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    -- Move focus to the next window (bis)
    -- depending on keyboard, more ergonomic mapping
    , ((mod1Mask,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the previous window (bis)
    , ((mod1Mask .|. shiftMask, xK_Tab   ), windows W.focusUp)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad -- NO!
--    , ((modm .|. shiftMask .|. mod2Mask .|. mod3Mask, xK_q     ), io (exitWith ExitSuccess))

--    , ((modm .|. shiftMask , xK_s ), spawn " slock & (sleep 1 ; sudo su -c \"echo mem > /sys/power/state\")" )
    , ((modm .|. shiftMask , xK_s ), spawn " slock " )

--    , ((modm .|. shiftMask , xK_x ), spawn "sudo su -c \"echo mem > /sys/power/state\"" )
-- "slock")

    -- perfect!
    , ((modm, xK_Escape), spawn "scrot -e 'mv $f ~/Screenshots'"  )

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask              , xK_q     ), spawn "emacs ~/.xmonad/xmonad.hs")

    , ((modm , xK_n),  addWorkspacePrompt myPrompt)
    , ((modm .|. shiftMask , xK_n), removeEmptyWorkspace)

    -- Cycle
    -- , ((), cycleRecentWS [] )
    , ((modm , xK_Left),  prevWS)
    , ((modm , xK_Right), nextWS)
    , ((modm .|. shiftMask , xK_Left),  shiftToPrev)
    , ((modm .|. shiftMask , xK_Right), shiftToNext)
    , ((modm , xK_Up),   prevScreen)
    , ((modm , xK_Down), nextScreen)
    , ((modm .|. shiftMask , xK_Up),   shiftPrevScreen)
    , ((modm .|. shiftMask , xK_Down), shiftNextScreen)
    
    
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- help not found 
    -- , ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[0..9], Switch to workspace N
    -- mod-shift-[0..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_0 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    -- mod-ctrl-[0..9], swap
    ++      
    [((modm .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_0 ..]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

-- had to swap these
--    -- mod-button2, Raise the window to the top of the stack
--    , ((modm, button3), (\w -> focus w >> windows W.shiftMaster))
--
-- this causes fucking freeze

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button2), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = minimize ( tiled ||| Mirror tiled ||| Full ||| threecol )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     threecol = ThreeCol nmaster delta ratio 

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Reference Net Workshop" --> doFloat
    , className =? "sun-awt-X11-XFramePeer" --> doFloat
    , className =? "de-renew-plugin-Loader" --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Vidalia"        --> doFloat
    , className =? "gui-worksheet"  --> doFloat
    , className =? "Emacs"	    --> doFloat
    , className =? "Pidgin"         --> doFloat
    , className =? "Inkscape"       --> doFloat
    , className =? "MuPDF"          --> doFloat
    , className =? "Gitk"           --> doFloat
    , className =? "Viewnior"       --> doFloat
--    , className =? "remac.py"	    --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , title     =? "Firefox Preferences" --> doFloat
    , title     =? "VisualizationPy" --> doFloat
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout, -- showWName myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
