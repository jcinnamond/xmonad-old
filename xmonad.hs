{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Circle
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.ManageHook

import XMonad.Util.EZConfig(additionalKeysP, removeKeysP, additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import ModalKeys

-- Some colors
-- From: https://terminal.sexy/
colorBg = "#1d1f21"
colorFg = "#c5c8c6"

color0 = "#282a2e"
color8 = "#373b41"
color1 = "#a54242"
color3 = "#de935f"
color6 = "#5e8d87"
color7 = "#707880"
color15 = "#c5c8c6"

-- Run xmobar on each screen
myPipe = spawnPipe (unwords ["xmobar"
                            , "~/.xmonad/xmobar.hs"
                            ])

main :: IO ()
main = do
  handle <- myPipe
  config <- withWindowNavigation (xK_k, xK_h, xK_j, xK_l) $ myConfig handle
  xmonad config

myConfig handle =
  docks
  $ def {
    modMask = mod1Mask
  , focusFollowsMouse = False
  , borderWidth = 4
  , focusedBorderColor = color1
  , normalBorderColor = color8
  , workspaces = myWorkspaces
  , layoutHook = myLayoutHook
  , logHook = dynamicLogWithPP $ myPP handle
  , manageHook = namedScratchpadManageHook myScratchpads
  }
  `removeKeysP` myRemoveKeys
  `additionalKeysP` myAdditionalKeys
  `additionalKeys` myLegacyKeys

-- Define workspaces
myWorkspaces = ["dev", "web", "chat", "x", "y", "config", "me", "music"]

-- Define scratchpads
myScratchpads =
  [ NS "todo" "~/bin/emacsclient -a='' -nc --frame-parameters='((name . \"emacstodo\"))' ~/todo.org"
    (title =? "emacstodo")
    (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "journal" "~/bin/emacsclient -a='' -nc --frame-parameters='((name . \"emacsjournal\"))' --eval '(org-journal-new-entry nil)'"
    (title =? "emacsjournal")
    (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]

-- Remove some key bindings to allow me to use Meta as the modMask without clashing with emacs too much
myRemoveKeys = [ "M-<Space>"
               , "M-S-<Return>"
               , "M-q"
               , "M-b"
               , "M-w", "M-e", "M-r"
               , "M-m"
               , "M-h"
               ]

-- Add some key bindings. A lot of the keys are delegated to the modalKeys popup
myAdditionalKeys = [ ("M-C-<Space>", modalKeys)
                     -- Some quick layout shortcuts
                   , ("M-C-l", sendMessage NextLayout)
                   , ("M-C-x", withFocused (sendMessage . maximizeRestore))
                   , ("M-C-k", kill)
                   , ("M-C-p", promote)

                   -- Cycle between windows and workspaces
                   , ("M-C-S-s", windows W.focusDown)
                   , ("M-C-S-j", prevWS)
                   , ("M-C-S-k", nextWS)

                   -- Scratchpads
                   , ("M4-<Space>", namedScratchpadAction myScratchpads "todo")
                   , ("C-M4-<Space>", namedScratchpadAction myScratchpads "journal")

                     -- Some multimedia keys
                   , ("<XF86AudioPlay>", spawn "playerctl play-pause")
                   , ("<XF86AudioPrev>", spawn "playerctl previous")
                   , ("<XF86AudioNext>", spawn "playerctl next")
                   , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
                   , ("<XF86MonBrightnessUp>", spawn "brightnessctl -d 'intel_backlight' set 20+")
                   , ("<XF86MonBrightnessDown>", spawn "brightnessctl -d 'intel_backlight' set 20-")
                   ]
                   ++
                   switchWorkspaceKeys

-- Extra keybindings for switching workspaces
switchWorkspaceKeys = [(otherModMasks ++ "M4-" ++ key, action tag)
                      | (tag, key)  <- zip myWorkspaces homeRow
                      , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                                   , ("S-", windows . W.shift)]
                      ]
  where homeRow = ["a","s","d","f","g","j","k","l"]

-- Keybindings that I haven't made work with additionalKeysP yet
myLegacyKeys = [ ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
               , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
               ]


myLayoutHook = maximize $ avoidStruts $ smartSpacing 7 $
               Tall 1 (3/100) (1/2)
               ||| Tall 1 (1/100) (3/4)
               ||| Circle


-- Configure xmobar
myPP h = xmobarPP { ppOutput = hPutStrLn h
                  , ppCurrent = xmobarColor color3 ""
                  , ppHidden = xmobarColor color7 "" . noScratchpads
                  , ppVisible = xmobarColor color3 ""
                  , ppHiddenNoWindows = xmobarColor color8 "" . noScratchpads
                  , ppTitle = xmobarColor colorFg ""
                  , ppLayout = \_ -> ""
                  , ppSep = "   <fc=#707880>|||</fc>   "
                  }
  where noScratchpads ws = if ws == "NSP" then "" else ws
