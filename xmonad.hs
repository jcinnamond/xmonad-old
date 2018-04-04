{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeysP, removeKeysP, additionalKeys)
import XMonad.Util.Run

import ModalKeys

-- Some colors
colorGrey = "#666666"
colorOrange = "#FF611A"

-- Run xmobar on each screen
myPipe screen = spawnPipe (unwords ["xmobar"
                                   , " -x ", (show screen)
                                   , "/home/jcinnamond/.xmonad/xmobar.hs"
                                   ])

main :: IO ()
main = do
  nScreens <- countScreens
  h0 <- myPipe 0
  h1 <- myPipe 1
  xmonad $ myConfig nScreens [h0, h1]

myConfig nScreens handles = docks $ def
  {
    modMask = mod1Mask
  , focusFollowsMouse = False
  , borderWidth = 2
  , focusedBorderColor = colorOrange
  , normalBorderColor = colorGrey
  , workspaces = withScreens nScreens (fmap show [1..9])
  , layoutHook = myLayoutHook
  , logHook = allPPs nScreens handles
  }
  `removeKeysP` myRemoveKeys
  `additionalKeysP` myAdditionalKeys nScreens
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
myAdditionalKeys nScreens = [ ("M-C-<Space>", modalKeys)
                              , ("M-j", sendMessage $ Go L)
                              , ("M-l", sendMessage $ Go R)
                              , ("M-i", sendMessage $ Go U)
                              , ("M-k", sendMessage $ Go D)

                              -- Some multimedia keys
                              , ("<XF86AudioPlay>", spawn "playerctl play-pause")
                              , ("<XF86AudioPrev>", spawn "playerctl previous")
                              , ("<XF86AudioNext>", spawn "playerctl next")
                              , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
                              , ("<XF86MonBrightnessUp>", spawn "brightnessctl -d 'intel_backlight' set 20+")
                              , ("<XF86MonBrightnessDown>", spawn "brightnessctl -d 'intel_backlight' set 20-")
                              ]
                              ++
                              -- Jump to displays
                              [
                                (mask ++ "M-" ++ [k], windows $ onCurrentScreen f i)
                              | (i, k) <- zip (workspaces' def) ['0'..'9']
                              , (f, mask) <- [ (W.view, ""), (W.shift, "S-")]
                              ]

-- Keybindings that I haven't made work with additionalKeysP yet
myLegacyKeys = [ ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
               , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
               ]


myLayoutHook = windowNavigation (avoidStruts (myGaps (Tall 1 (3/100) (1/2))))
               ||| Circle
               ||| Full
  where myGaps = lessBorders OnlyFloat . smartSpacing 7


-- Configure xmobar
allPPs nScreens handles = sequence_ [dynamicLogWithPP $ marshallPP s $ myPP h | s <- [0..nScreens-1], h <- handles]

myPP h = xmobarPP { ppOutput = hPutStrLn h
                  , ppCurrent = xmobarColor colorOrange ""
                  , ppHidden = xmobarColor "white" ""
                  , ppVisible = xmobarColor colorOrange ""
                  , ppHiddenNoWindows = xmobarColor colorGrey ""
                  , ppTitle = xmobarColor "white" ""
                  , ppLayout = \_ -> ""
                  , ppSep = " | "
                  }
