--imports/sources
--system

import XMonad 
import XMonad.Layout
import XMonad.ManageHook (className)
import Data.Monoid
import System.Exit (exitWith, exitSuccess)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Prompt.ConfirmPrompt
import XMonad.Config.Xfce (xfceConfig)

--utils

import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.CustomKeys
import XMonad.Util.Dmenu
import XMonad.Util.NamedScratchpad

-- layout

import XMonad.Layout.Spacing (spacing, smartSpacing)
import XMonad.Layout.LayoutCombinators(JumpToLayout)
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ZoomRow
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Renamed(renamed, Rename(Replace))

-- hooks

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.ManageHelpers

-- actions

import XMonad.Actions.Submap
import XMonad.Actions.CycleWS

-- qualified

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Util.Run as Run (safeSpawn, spawnPipe)

-- main config

logMessage :: String -> X ()
logMessage msg = spawn $ "echo '" ++ msg ++ "' >> /tmp/xmonad.log"

main :: IO ()
main = do
  xmonad $ ewmhFullscreen $ ewmh $ docks $ myConfig
  where
myBar = "polybar"

myConfig = def
      { modMask = mod4Mask
      , focusFollowsMouse = True
      , clickJustFocuses = False
      , borderWidth = 2
      , focusedBorderColor = "#ffa384"
      , normalBorderColor = "#e7f2f8"
      , layoutHook = myLayoutHook 
      , keys = myKeys
      , workspaces = myWorkspaces
      , manageHook = myManageHook 
      , startupHook = myStartupHook
      , handleEventHook = handleEventHook def
      }

myBrowser :: String
myBrowser = "firefox"

myTerminal :: String
myTerminal = "st -e tmux"

myDmenu :: String
myDmenu = "dmenu_run"

myScreenshot :: String
myScreenshot = "flameshot gui"

myWorkspaces = ["1_dev", "2_www", "3_ide", "4_im", "5_stm", "6_doc", "7_vrt", "8_msc", "9_msu"]

mySoundDir :: String
mySoundDir = "/opt/sounds/"

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Krita" --> doFloat
    , className =? "Xmessage" --> doFloat
    , className =? "steam" --> doFloat
    , className =? "error" --> doFloat
    , className =? "dialog" --> doFloat
    , className =? "message" --> doFloat
    , className =? "file_progress" --> doFloat
    , className =? "scratchpad" --> doFloat
    , className =? "spotify" --> doFloat
    , className =? "leafpad" --> doFloat
    , title =? "Xfce4-whiskermenu-popup" --> doFloat 
    , manageDocks
    ] <+> namedScratchpadManageHook myScratchpads 

-- startup 

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "polybar &"
  spawnOnce "picom &"
  spawnOnce "xscreensaver -no-splash &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "xfce4-session"

myScratchpads = 
    [ NS "terminal" spawnPad findPad managePad
    , NS "spotify" "spotify" (className =? "spotify") manageSpotify
    , NS "leafpad" "leafpad" (className =? "leafpad") manageLeafpad
    ]
    where
        spawnPad  = "st -c scratchpad"
        findPad   = className =? "scratchpad"
        managePad = customFloating $ W.RationalRect (1 / 2) (1 /2) (1 / 2) (1 / 2)
       
        manageSpotify = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
        
        manageLeafpad = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)

-- floating windows

toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else (W.float w (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)) s)
    )

-- layouts

myLayoutHook = avoidStruts $ smartBorders $ myLayout
  where
    myLayout =  spacing 15 
              $ renamed [Replace "Mirror"] (smartSpacing 15 $ Mirror (ResizableTall 1 (3/100) (1/2) []))
            ||| renamed [Replace "ThreeColMid"] (smartSpacing 10 $ ThreeColMid 1 (3/100) (1/2))
            ||| renamed [Replace "emptyBSP"] (smartSpacing 10 $ emptyBSP)
            ||| renamed [Replace "mouseResizableTile"] (smartSpacing 10 $ mouseResizableTile)
            ||| renamed [Replace "zoomRow"] (smartSpacing 10 $ zoomRow) 

-- keybinds

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
     
-- xmonad keybindings
    [ ((modm,                   xK_Return), spawn myTerminal)
    , ((modm .|.  shiftMask,    xK_q), spawn "xmonad --recompile")
    , ((modm .|.  shiftMask,    xK_c), kill)      
    , ((modm .|.  shiftMask,    xK_p), spawn myDmenu)
    , ((modm .|.  shiftMask,    xK_s), spawn myScreenshot)
    , ((modm .|.  shiftMask,    xK_k), spawn "rofi -show drun")
    , ((modm,                   xK_q), do
    spawn $ "mpv --no-video " ++ mySoundDir ++ "chord.wav" 
    confirmPrompt def "exit" $ spawn "killall Xorg"
    )
-- layout keybinds
    , ((modm,                   xK_f), nextWS)
    , ((modm,                   xK_d), prevWS)
    , ((modm,                   xK_g), sendMessage $ JumpToLayout "Mirror")
    , ((modm,                   xK_h), sendMessage $ JumpToLayout "ThreeColMid")
    , ((modm,                   xK_j), sendMessage $ JumpToLayout "emptyBSP")
    , ((modm,                   xK_k), sendMessage $ JumpToLayout "mouseResizableTile")
    , ((modm,                   xK_l), sendMessage $ JumpToLayout "zoomRow")
    , ((modm .|. shiftMask, xK_space), sendMessage NextLayout)
    , ((modm,               xK_space), withFocused $ toggleFloat)
    , ((modm,                 xK_Tab), windows W.focusDown)
    , ((modm .|. shiftMask,     xK_e), sendMessage Shrink)
    , ((modm .|. shiftMask,     xK_d), sendMessage Expand)
-- scratchpad keybinds
    , ((mod1Mask,               xK_t), namedScratchpadAction myScratchpads "terminal")
    , ((mod1Mask,               xK_m), namedScratchpadAction myScratchpads "spotify")
    , ((mod1Mask,               xK_l), namedScratchpadAction myScratchpads "leafpad")
-- workspace keybinds
    , ((modm .|. shiftMask, xK_1), windows $ W.shift $ myWorkspaces !! 0)
    , ((modm .|. shiftMask, xK_2), windows $ W.shift $ myWorkspaces !! 1)
    , ((modm .|. shiftMask, xK_3), windows $ W.shift $ myWorkspaces !! 2)
    , ((modm .|. shiftMask, xK_4), windows $ W.shift $ myWorkspaces !! 3)
    , ((modm .|. shiftMask, xK_5), windows $ W.shift $ myWorkspaces !! 4)
    , ((modm .|. shiftMask, xK_6), windows $ W.shift $ myWorkspaces !! 5)
    , ((modm .|. shiftMask, xK_7), windows $ W.shift $ myWorkspaces !! 6)
    , ((modm .|. shiftMask, xK_8), windows $ W.shift $ myWorkspaces !! 7)
    , ((modm .|. shiftMask, xK_9), windows $ W.shift $ myWorkspaces !! 8)
    , ((modm,               xK_1), windows $ W.greedyView $ myWorkspaces !! 0)
    , ((modm,               xK_2), windows $ W.greedyView $ myWorkspaces !! 1)
    , ((modm,               xK_3), windows $ W.greedyView $ myWorkspaces !! 2)
    , ((modm,               xK_4), windows $ W.greedyView $ myWorkspaces !! 3)
    , ((modm,               xK_5), windows $ W.greedyView $ myWorkspaces !! 4)
    , ((modm,               xK_6), windows $ W.greedyView $ myWorkspaces !! 5)
    , ((modm,               xK_7), windows $ W.greedyView $ myWorkspaces !! 6)
    , ((modm,               xK_8), windows $ W.greedyView $ myWorkspaces !! 7)
    , ((modm,               xK_9), windows $ W.greedyView $ myWorkspaces !! 8)
    ]
