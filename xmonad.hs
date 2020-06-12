{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import XMonad
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeysP, removeKeysP, additionalKeys)
import XMonad.Util.Run

import ModalKeys

-- Some colors
colorGrey = "#808a87"
colorOrange = "#f73a18"
colorRed = "#74281a"

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
  , focusedBorderColor = colorRed
  , normalBorderColor = colorGrey
  , workspaces = fmap show [1..9]
  , layoutHook = myLayoutHook
  , logHook = dynamicLogWithPP $ myPP handle
  }
  `removeKeysP` myRemoveKeys
  `additionalKeysP` myAdditionalKeys
  `additionalKeys` myLegacyKeys

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

                     -- Some multimedia keys
                   , ("<XF86AudioPlay>", spawn "playerctl play-pause")
                   , ("<XF86AudioPrev>", spawn "playerctl previous")
                   , ("<XF86AudioNext>", spawn "playerctl next")
                   , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
                   , ("<XF86MonBrightnessUp>", spawn "brightnessctl -d 'intel_backlight' set 20+")
                   , ("<XF86MonBrightnessDown>", spawn "brightnessctl -d 'intel_backlight' set 20-")
                   ]

-- Keybindings that I haven't made work with additionalKeysP yet
myLegacyKeys = [ ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
               , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
               ]


myLayoutHook = maximize $ avoidStruts (myGaps (Tall 1 (3/100) (1/2)))
               ||| Circle
	       ||| avoidStruts (myGaps (Tall 1 (10/100) (4/5)))
  where myGaps = smartSpacing 7


-- Configure xmobar
myPP h = xmobarPP { ppOutput = hPutStrLn h
                  , ppCurrent = xmobarColor colorOrange ""
                  , ppHidden = xmobarColor "white" ""
                  , ppVisible = xmobarColor colorOrange ""
                  , ppHiddenNoWindows = xmobarColor colorGrey ""
                  , ppTitle = xmobarColor "white" ""
                  , ppLayout = \_ -> ""
                  , ppSep = " | "
                  }
