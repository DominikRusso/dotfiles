import System.Exit
import XMonad
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad defaultConfig
  {
    borderWidth        = borderWidth'
  , focusedBorderColor = focusedBorderColor'
  , keys               = keys'
  , layoutHook         = layouts'
  , modMask            = modMask'
  , normalBorderColor  = normalBorderColor'
  , workspaces         = workspaces'
  }

modMask'            = mod4Mask
workspaces'         = map show [1..9]

borderWidth'        = 1
focusedBorderColor' = "#aaaaaa"
normalBorderColor'  = "#222222"

-------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------

keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [
    ((modm .|. shiftMask, xK_g), decScreenWindowSpacing 2)
  , ((modm,               xK_g), incScreenWindowSpacing 2)
  , ((modm .|. mod1Mask,  xK_g), sequence_ [toggleScreenSpacingEnabled, toggleWindowSpacingEnabled])
  , ((modm,               xK_h), sendMessage Shrink)
  , ((modm .|. shiftMask, xK_i), sendMessage (IncMasterN (-1)))
  , ((modm              , xK_i), sendMessage (IncMasterN 1))
  , ((modm,               xK_j), windows W.focusDown)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  , ((modm,               xK_k), windows W.focusUp)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm,               xK_l), sendMessage Expand)
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  , ((modm,               xK_q), kill)
  , ((modm,               xK_space), sendMessage NextLayout)
  ]

  ++

  -- mod-[1..9]       switch to workspace N
  -- mod-shift-[1..9] move active window to workspace N
  [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]


-------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------

layouts' = masterStack ||| monocle

masterStack = renamed [Replace "[]≡"] $ spacingRaw False (uniBorder 5) True (uniBorder 5) True $ Tall 1 (5/100) (1/2)
monocle     = renamed [Replace "[ ]"] $ Full

-- construct a uniform 'Border'
uniBorder :: Integer -> Border
uniBorder i = Border i i i i

