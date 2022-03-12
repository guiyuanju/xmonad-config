import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Spacing

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


myConfig =
  def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , terminal = "alacritty"
    , borderWidth = 3
    , focusedBorderColor = "#bd93f9" 
    -- , focusedBorderColor = "#007acc"
    , startupHook = myStartupHook
    , manageHook = myManageHook
    } `additionalKeysP`
  [ ("M-S-z", spawn "xscreensaver-command -lock")
  , ("M-S-=", unGrab *> spawn "scrot -s")
  , ("M-b", spawn "firefox")
  , ("M-<Space>", spawn "rofi -show combi")
  , ("M-e", spawn "emacsclient -c")
  , ("M-C-<Return>", spawn "emacsclient -c")
  , ("<F11>", spawn "amixer set Master 5%-")
  , ("<F12>", spawn "amixer set Master 5%+")
  , ("<F10>", spawn "amixer set Master 0%")
  ]

  
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]


myStartupHook = do
  -- spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent false --tint 0x5f5f5f --height 43"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "xscreensaver -no-splash"
  -- spawnOnce "xfce4-power-manager"
  -- spawnOnce "nm-applet --sm-disable"
  spawnOnce "feh --bg-scale /usr/share/backgrounds/wallpapers-2018/mountains-1412683.jpg"
  spawnOnce "redshift -l 35.60843:139.68874"
  spawnOnce "picom"
  spawnOnce "emacs --daemon"
  -- spawnOnce "syncthingtray"

mySpacing = spacingRaw False            -- False=Apply even when single window
                       (Border 8 8 8 8) -- Screen border size top bot
                                        -- rght lft
                       True             -- Enable screen border
                       (Border 8 8 8 8) -- Window border size
                       True             -- Enable window borders
  
  
myLayout = mySpacing $ tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- number of master pane
    ratio = 1/2 -- master pane size
    delta = 3/100 -- resize percentage

    
myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = white " | "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#bd93f9" 2
    , ppHidden = wrap " " "" . white
    , ppHiddenNoWindows = wrap " " "" . lowWhite
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, _, _, wins] -> [ws, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    -- formatFocused = wrap (white "[") (white "]") . blue . ppWindow
    -- formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . lowWhite . ppWindow
    -- formatFocused = wrap (white "") (white "") . blue . ppWindow
    -- formatUnfocused = wrap (lowWhite "") (lowWhite "") . lowWhite . ppWindow
    formatFocused = wrap "" "" . xmobarBorder "Full" "#222222" 4 . xmobarColor "#000000" "#bd93f9" . wrap " " " " . ppWindow
    formatUnfocused = wrap "" "" . xmobarBorder "Full" "#222222" 4 . lowWhite . wrap " " " " . ppWindow
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
