import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.XPropManage
import XMonad.Layout.Spacing
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Drawer
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import Data.List
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDebug
import XMonad.Layout.Hidden
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ResizableTile
import XMonad.Layout.Maximize
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Renamed
import XMonad.Actions.TagWindows
import XMonad.Prompt    -- to use tagPrompt
-- import XMonad.Prompt.Shell
import XMonad.Prompt.FuzzyMatch
-- import XMonad.Prompt.RunOrRaise
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import qualified XMonad.Actions.CycleWS as CWS
import XMonad.Actions.GroupNavigation
import XMonad.Hooks.RefocusLast
-- import XMonad.Layout.Simplest
-- bar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers


main :: IO ()
main =
  xmonad $
  ewmhFullscreen $
  ewmh $
  withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) toggleStrutsKey $ myConfig
   where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_B)

myTerminal = "alacritty"

myConfig =
  def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , terminal = myTerminal
    , borderWidth = 10
    -- , focusedBorderColor = "#bd93f9"
    , focusedBorderColor = "#FBB040"
    -- , focusedBorderColor = "#f5d7a1"
    , normalBorderColor = "#444444"
    -- , unfocusedBorderColor = "#007acc"
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    -- , logHook = myLogHook xmobars >> historyHook
    , logHook = refocusLastLogHook >> nsHideOnFocusLoss myScratchpads >> historyHook -- enable hiding for all of @myScratchpads@
    -- , logHook = historyHook
    } `additionalKeysP`
  myKeybindings

myKeybindings =
  [ ("M-S-z", spawn "xscreensaver-command -lock")
  , ("M-C-1", unGrab *> spawn "scrot '/home/las/Downloads/%Y-%m-%d_$wx$h.jpg' -s -q 50")
  , ("M-b", spawn "firefox")
  , ("M-<Space>", spawn "rofi -show combi")
  , ("M-d", spawn "ulauncher-toggle")
  , ("M-p", spawn "cmd.clj")
  , ("M-S-<Space>", sendMessage NextLayout)
  , ("M-e", spawn "emacsclient -c")
  , ("M-C-<Return>", spawn "emacsclient -c")
  , ("<F11>", spawn "pamixer -d 5")
  , ("<F12>", spawn "pamixer -i 5")
  , ("<F10>", spawn "pamixer -t")
  , ("M-m", windows W.focusMaster)
  , ("M-;", withFocused hideWindow)
  , ("M-S-;", popOldestHiddenWindow)
  -- , ("M-a", sendMessage MirrorShrink)
  -- , ("M-z", sendMessage MirrorExpand)
  , ("M-f", withFocused (sendMessage . maximizeRestore))
  -- , ("M-S-e", spawn "emacsclient --eval  \"(emacs-everywhere)\"")
  -- , ("M-<Space>", shellPrompt myXPConfig)
  -- , ("M-S-d", runOrRaisePrompt myXPConfig)
  -- , ("M-'", tagPrompt myXPConfig (\s -> withFocused (addTag s)))
  -- , ("M-S-'", tagDelPrompt myXPConfig)
  , ("M-'", myRemoveOrAddTag >> windows W.focusMaster)
  -- , ("M-S-'", withFocused (delTag "drawer") >> windows W.focusMaster)
  , ("M-x", namedScratchpadAction myScratchpads "note")
  -- , ("M-x", namedScratchpadAction myScratchpads "note")
  , ("M-z", namedScratchpadAction myScratchpads "terminal")
  , ("M-c", namedScratchpadAction myScratchpads "time-tracking")
  , ("M-S-t", withFocused $ windows . W.sink)
  , ("M-<Right>", CWS.nextWS)
  , ("M-<Left>", CWS.prevWS)
  , ("M-S-`", CWS.toggleWS)
  , ("M-`", nextMatch History (return True))
  ]

myDrawerTag = "drawer"
myRemoveOrAddTag = withFocused (\w -> do
                                   isExist <- hasTag myDrawerTag w
                                   if isExist then delTag myDrawerTag w else addTag myDrawerTag w)
myXPConfig =
  def
    { font = "xft:fira code:size=12"
    , bgColor = myColorBg
    , fgColor = myColorFg
    , bgHLight = myColorBgAlt
    , fgHLight = myColorFgAlt
    , borderColor = myColorBg
    , promptBorderWidth = 0
    , promptKeymap = emacsLikeXPKeymap
    , position = CenteredAt 0.36 0.5 -- vertical position and width
    , height = 60
    , historySize = 256
    , historyFilter = id
    , defaultText = ""
    -- , autoComplete = Nothing
    , showCompletionOnTab = False
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , alwaysHighlight = True
    , maxComplRows = Just 10 -- or Nothing
    , changeModeKey = xK_grave
    }

-- myXPKeymap = mkKeymap myConfig [("C-n", )]

myWorkspaces = ["res", "gdev", "dev", "edu", "ai", "6", "7", "8", "tools"]

(~~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~~? x = fmap (x `isInfixOf`) q

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog --> doFloat
    -- , (stringProperty "wM_CLASS") ~~? "Godot_Engine" --> doFloat
    , className =? "confirm" --> doFloat
    , className =? "file_progress" --> doFloat
    , className =? "dialog" --> doFloat
    , className =? "download" --> doFloat
    , className =? "error" --> doFloat
    , className =? "notification" --> doFloat
    , className =? "pinentry-gtk-2" --> doFloat
    , className =? "splash" --> doFloat
    , className =? "toolbar" --> doFloat
    , className =? "copyq" --> doFloat
    , className =? "Godot" --> doFloat
    , transience'
    -- , className ~~? "(DEBUG)" --> doFloat
    , namedScratchpadManageHook myScratchpads
    ]

myScratchpads =
  [ NS "note" "logseq" (className =? "Logseq") bigFloatCentered
  , NS
      "terminal"
      (myTerminal ++ " -t scratchpad")
      (title =? "scratchpad")
      bigFloatCentered
  , NS
      "time-tracking"
      "clockify"
      (className =? "Clockify")
      bigFloatCentered
  ]
  where
    bigFloatCentered = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w


myStartupHook = do
  -- spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent false --tint 0x5f5f5f --height 43"
  spawnOnce "xsetroot -cursor_name left_ptr &"
  -- spawnOnce "xscreensaver -no-splash &"
  -- spawnOnce "xfce4-power-manager"
  -- spawnOnce "nm-applet --sm-disable"
  spawnOnce "feh --bg-scale /usr/share/backgrounds/wallpapers-2018/mountains-1412683.jpg &"
  spawnOnce "redshift -l 35.60843:139.68874 &"
  spawnOnce "picom &"
  spawnOnce "emacs --daemon &"
  spawnOnce "fcitx5 &"
  spawnOnce "ulauncher"
  -- spawnOnce "flashfocus &" -- need flashfocus and picom to be installed
  -- spawnOnce "syncthingtray"

-- mySpacing = spacingRaw False            -- False=Apply even when single window
--                        (Border 8 8 8 8) -- Screen border size top bot
--                                         -- rght lft
--                        True             -- Enable screen border
--                        (Border 8 8 8 8) -- Window border size
--                        True             -- Enable window borders
  
  
myLayout = renamed [CutWordsLeft 5] $ hiddenWindows $ maximize $ myDrawer $ mySpacing $ tiled ||| full
  where
    -- float = renamed [Replace "float"] simplestFloat
    full = renamed [Replace "full"] $ noBorders Full
    tiled = renamed [Replace "tiled"]
      mouseResizableTile
        {masterFrac = 0.5, fracIncrement = 0.05, draggerType = BordersDragger}
    mySpacing = smartSpacingWithEdge 8
    myDrawer = onRight $ simpleDrawer 0.00015 0.45 (Tagged myDrawerTag `Or` ClassName "Xchat")

-- myLayout = maximize $ smartBorders $ hiddenWindows 
--   $ tiled ||| full
--   where
--     nmaster = 1 -- number of master pane
--     ratio = 1 / 2 -- master pane size
--     delta = 3 / 100 -- resize percentage
--     tiled = drawer' `onRight` (smartSpacingWithEdge 8 $ ResizableTall nmaster delta ratio [])
--     full = drawer' `onRight` (smartSpacingWithEdge 8 $ Full)
--     -- threeCol = ThreeColMid nmaster delta ratio
--     -- threeCol = magnifiercz 1.5 $ ThreeColMid nmaster delta ratio
--     -- reflectedTiled = reflectHoriz tiled
--     drawer' = drawer 0.0001 0.4 (Tagged myDrawerTag `Or` ClassName "Xchat") $ ResizableTall 0 delta 0 []
myXmobarPP :: PP
myXmobarPP =
  filterOutWsPP ["NSP"] $ def
    { ppSep = white " | "
    , ppWsSep = " "
    , ppTitleSanitize = xmobarStrip
    -- , ppCurrent = wrap " " "" . xmobarBorder "Top" "#bd93f9" 2
    , ppCurrent = wrap "[" "]"
    , ppHidden = wrap " " " " . white
    , ppHiddenNoWindows = wrap " " " " . lowWhite
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, layout, cw] -> [ws, layout]
    -- , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    -- formatFocused = wrap (white "[") (white "]") . blue . ppWindow
    -- formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . lowWhite . ppWindow
    -- formatFocused = wrap (white "") (white "") . blue . ppWindow
    -- formatUnfocused = wrap (lowWhite "") (lowWhite "") . lowWhite . ppWindow
    -- formatFocused = wrap "" "" . xmobarBorder "Full" "#222222" 4 . xmobarColor "#000000" "#bd93f9" . wrap " " " " . ppWindow
    formatFocused = wrap "" "" . wrap "[" "]" . ppWindow
    -- formatUnfocused = wrap "" "" . xmobarBorder "Full" "#222222" 4 . lowWhite . wrap " " " " . ppWindow
    formatUnfocused = wrap "" "" . wrap " " " " . ppWindow
    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow =
      xmobarRaw .
      (\w ->
         if null w
           then "untitled"
           else w) .
      shorten' "~" 10
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myColorBg = "#222222" -- black
myColorBgAlt = myColor1 -- gray
myColorFg = "#eeeeee" -- white
myColorFgAlt = myColorBg
-- myColorPri = "#bd93f9" -- purple
-- myColorAlt = "#ADD8E6" -- light blue
-- myColorHighlight = "#ff79c6"

myColor1 = "#bd93f9" -- purple
myColor2 = "#686CA5" -- blue
myColor3 = "#EDCCCE" -- pink
myColor4 = "#A08274" -- brown
