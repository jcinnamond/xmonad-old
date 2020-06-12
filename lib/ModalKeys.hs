module ModalKeys (modalKeys) where

import Data.Map as M
import GHC.Int (Int32)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote (promote)
import XMonad.Actions.Submap
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Prompt (XPConfig, font, position, XPPosition(Top), height, bgColor, fgColor, fgHLight, bgHLight)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.StackSet (current,screenDetail,sink)
import XMonad.Util.Font
import XMonad.Util.XUtils

longestLine' :: [[Char]] -> Display -> XMonadFont -> Int -> X Int
longestLine' [] _ _ l = return l
longestLine' (x:xs) d f l = do
  nl <- textWidthXMF d f x
  longestLine' xs d f (max nl l)

longestLine :: [[Char]] -> Display -> XMonadFont -> X Int
longestLine strs d f = longestLine' strs d f 0

writeStrings :: Display -> Pixmap -> XMonadFont -> GC -> GHC.Int.Int32 -> [[Char]] -> GHC.Int.Int32 -> X ()
writeStrings _ _ _ _ _ [] _ = return ()
writeStrings d p f gc lh (x:xs) y = do
  printStringXMF d p f gc "white" "gray10" 15 (fi y) x
  writeStrings d p f gc lh xs (y + lh)

showGuideKeys :: [[Char]] -> X Window
showGuideKeys strs = do
  d <- asks display
  dim <- gets $ screenRect . screenDetail . current . windowset
  f <- initXMF "xft:Input Pro:pixelsize=20"
  lgn <- longestLine strs d f
  (as,ds) <- textExtentsXMF f (head strs)
  let lh = as + ds
      wth = (fi lgn) + 30
      hgt = lh * (fi (length strs)) + 30
      y = (rect_y dim) + (fi (rect_height dim) - hgt) `div` 2
      x = (rect_x dim) + (fi (rect_width dim) - (fi wth)) `div` 2

  w <- createNewWindow (Rectangle (fi x) (fi y) wth (fi hgt)) Nothing "" True
  showWindow w
  p <- io $ createPixmap d w wth (fi hgt) (defaultDepthOfScreen $ defaultScreenOfDisplay d)
  gc <- io $ createGC d p
  c' <- stringToPixel d "gray10"
  io $ setForeground d gc c'
  io $ fillRectangle d p gc 0 0 wth (fi hgt)
  writeStrings d p f gc lh strs (lh + 15)
  io $ copyArea      d p w gc 0 0 wth (fi hgt) 0 0
  -- free the pixmap and GC
  io $ freePixmap    d p
  io $ freeGC        d gc
  releaseXMF f
  return w

modalKeys :: X ()
modalKeys = do
  w <- showGuideKeys [ "screen:         ⟵ [w]  [e] ⟶"
                     , "workspace:   ⟵ [j]  [l] ⟶"
                     , ""
                     , "[f] change layout               [return] promote"
                     , "[,] move to master             [.] remove from master"
                     , "[s] maximize"
                     , ""
                     , "[c] kill current window       [j] unfloat current window"
                     , ""
                     , "[l] screen capture"
                     , ""
                     , "[space] applications"
                     , ""
                     , "[k] lock                               [q] restart"
                     , "[m] sort out monitors"
                     ]

  submapDefault (deleteWindow w) . M.fromList $
    [ ((0, xK_w), deleteWindow w >> prevScreen)
    , ((mod1Mask, xK_w), deleteWindow w >> shiftPrevScreen >> prevScreen)
    , ((0, xK_e), deleteWindow w >> nextScreen)
    , ((mod1Mask, xK_e), deleteWindow w >> shiftNextScreen >> nextScreen)
    , ((0, xK_j), deleteWindow w >> prevWS)
    , ((0, xK_l), deleteWindow w >> nextWS)
    , ((0, xK_Return), deleteWindow w >> promote)
    , ((0, xK_f), deleteWindow w >> sendMessage NextLayout)
    , ((0, xK_s), deleteWindow w >> withFocused (sendMessage . maximizeRestore))
    , ((0, xK_comma), deleteWindow w >> sendMessage (IncMasterN 1))
    , ((0, xK_period), deleteWindow w >> sendMessage (IncMasterN (-1)))
    , ((0, xK_c), deleteWindow w >> kill)
    , ((0, xK_j), deleteWindow w >> withFocused (windows . sink))
    , ((0, xK_l), deleteWindow w >> spawn "flameshot gui")
    , ((0, xK_space), deleteWindow w >> appKeys)
    , ((mod1Mask .|. controlMask, xK_space), deleteWindow w >> appKeys)
    , ((0, xK_p), deleteWindow w >> xmonadPrompt myXPConfig)
    , ((0, xK_k), deleteWindow w >> spawn "cinnamon-screensaver-command -l")
    , ((0, xK_m), deleteWindow w >> monitorKeys)
    , ((0, xK_q), restart "xmonad" True)
    ]

appKeys :: X ()
appKeys = do
  w <- showGuideKeys [ "              [space] launcher"
                     , ""
                     , "[t] xterm     [e] emacs     [w] web"
                     ]
  submapDefault (deleteWindow w) . M.fromList $
    [ ((0, xK_space), deleteWindow w >> shellPrompt myXPConfig)
    , ((mod1Mask .|. controlMask, xK_space), deleteWindow w >> shellPrompt myXPConfig)
    , ((0, xK_t), deleteWindow w >> spawn "xterm")
    , ((0, xK_e), deleteWindow w >> spawn "emacsclient -ca emacs")
    , ((0, xK_w), deleteWindow w >> spawn "firefox --new-instance --ProfileManager")]

monitorKeys :: X ()
monitorKeys = do
  w <- showGuideKeys [ "[m] single monitor   [f] mirror external   [j] mirror laptop" ]
  submapDefault (deleteWindow w) . M.fromList $
    [ ((0, xK_m), deleteWindow w >> spawn "xrandr --output eDP-1-1 --off --output DP-1-3 --auto")
    , ((0, xK_f), deleteWindow w >> spawn "xrandr --output eDP-1-1 --auto --same-as DP-1-3 --scale-from 2560x1440")
    , ((0, xK_j), deleteWindow w >> spawn "xrandr --output eDP-1-1 --auto")]

myXPConfig :: XPConfig
myXPConfig = def { font = "xft:firacode:pixelsize=18:autohint=true"
                 , position = Top
                 , height = 30
                 , bgColor = "#282a2e"
                 , fgColor = "#c6c8c6"
                 , fgHLight = "#de935f"
                 , bgHLight = "#1d1f21"
                 }
