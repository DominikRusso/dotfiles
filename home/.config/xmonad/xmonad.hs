import Data.List (find)
import System.Exit
import XMonad hiding ( (|||) )
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.Run
import XMonad.Util.Types

import qualified Data.Map        as M
import qualified XMonad.StackSet as W


-------------------------------------------------------------------
-- Main
-------------------------------------------------------------------

main :: IO ()
main = do
  nScreens <- countScreens
  xmobars  <- mapM (spawnPipe . xmobarCommand) [0..nScreens-1]
  xmonad $ docks def
    { borderWidth        = borderWidth'
    , focusedBorderColor = focusColor
    , focusFollowsMouse  = focusFollowsMouse'
    , keys               = keys'
    , layoutHook         = layouts'
    , logHook            = mapM_ dynamicLogWithPP $ zipWith logHook' xmobars [0..]
    , modMask            = mainM
    , normalBorderColor  = normalColor
    , terminal           = terminal'
    , workspaces         = withScreens nScreens ws'
    }


-------------------------------------------------------------------
-- General Config
-------------------------------------------------------------------

focusColor          = "green"
normalColor         = "black"

borderWidth'        = 1
gapSize             = 4

terminal'           = "alacritty"
ws'                 = map show [1..4]

focusFollowsMouse'  = True
clickJustFocuses    = False


-------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------

ctrl, meta, shift, super :: KeyMask
meta  = mod1Mask
ctrl  = controlMask
shift = shiftMask
super = mod4Mask

mainM   = meta  -- main modifier key
screenM = super -- modifier for things related to screens
moveM   = shift -- modifier for things related to moving (shifting)

keys' conf@(XConfig {}) = M.fromList $
  [
  -- navigating windows
    ((mainM, xK_j), windows W.focusDown)
  , ((mainM, xK_k), windows W.focusUp)
  , ((mainM, xK_m), windows W.focusMaster)

  -- navigating screens
  , ((screenM, xK_k), onPrevNeighbour def W.view)
  , ((screenM, xK_j), onNextNeighbour def W.view)

  -- moving windows on screens
  , ((mainM .|. moveM, xK_j), windows W.swapDown)
  , ((mainM .|. moveM, xK_k), windows W.swapUp)
  , ((mainM .|. moveM, xK_m), windows W.swapMaster)
  , ((mainM,           xK_h), sendMessage Shrink)
  , ((mainM,           xK_l), sendMessage Expand)
  , ((mainM,           xK_t), withFocused $ windows . W.sink) -- (t)ile floating

  -- moving windows between screens
  , ((screenM .|. moveM, xK_k), onPrevNeighbour def W.shift)
  , ((screenM .|. moveM, xK_j), onNextNeighbour def W.shift)

  -- controlling gaps and padding
  , ((mainM, xK_g), sequence_ [toggleScreenSpacingEnabled,  -- (g)aps
                               toggleWindowSpacingEnabled])
  , ((mainM, xK_b), sendMessage ToggleStruts)               -- (b)ar

  -- layouts
  , ((mainM, xK_Tab), sendMessage NextLayout)

  -- opening and closing programs
  , ((mainM .|. ctrl,  xK_Escape), io (exitWith ExitSuccess))
  , ((mainM .|. ctrl,  xK_r     ), spawn "xmonad --recompile && xmonad --restart")
  , ((mainM .|. moveM, xK_q     ), kill)
  , ((mainM,           xK_Return), spawn "alacritty")
  , ((mainM,           xK_d     ), spawn "dmenu_run")
  , ((mainM,           xK_o     ), submap . M.fromList $
      -- (o)pen
      [ ((0, xK_a), spawn "alacritty -e alsamixer")
      , ((0, xK_b), spawn "qutebrowser")
      , ((0, xK_c), spawn "alacritty -e calcurse")
      , ((0, xK_h), spawn "alacritty -e htop")
      , ((0, xK_m), spawn "alacritty -e cmus")
      , ((0, xK_n), spawn "alacritty -e newsboat")
      , ((0, xK_v), spawn "alacritty -e nvim")
      ])

  -- system control
  , ((mainM,           xK_p), spawn "scrot ~/media/images/screenshots/screenshot-%Y-%m-%d-%T.png")
  , ((mainM .|. moveM, xK_p), spawn "scrot -s ~/media/images/screenshots/screenshot-%Y-%m-%d-%T.png")
  , ((mainM,           xK_x), spawn "physlock -m -s")
  , ((mainM,           xK_s), submap . M.fromList $
      -- (s)ystem
      [ ((0, xK_h), spawn "dmenu_prompt \"Hibernate?\" \"systemctl hibernate\"") -- suspend to disk
      , ((0, xK_r), spawn "dmenu_prompt \"Reboot?\" \"sudo -A reboot\"")
      , ((0, xK_s), spawn "dmenu_prompt \"Shutdown?\" \"sudo -A shutdown -h now\"")
      , ((0, xK_z), spawn "systemctl suspend") -- suspend to swap (z)zz
      ])
  ]

  ++

  -- mod-[1..k]       move focus to workspace N
  -- mod-shift-[1..k] move window to workspace N
  [((mask .|. mainM, key), windows $ onCurrentScreen f ws)
    | (ws, key) <- zip (workspaces' conf) [xK_1..]
    , (f, mask) <- [(W.view, 0), (W.shift, moveM)]
  ]

  ++

  -- screenM-{1,2,3}       move focus to screen 1, 2, or 3
  -- screenM-shift-{1,2,3} move window to screen 1, 2, or 3
  [((mask .|. screenM, key), f screen)
    | (key, screen) <- zip [xK_1, xK_2, xK_3] [0..]
    , (f, mask)     <- [(viewScreen def, 0), (sendToScreen def, moveM)]
  ]


-------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------

layouts' = avoidStruts     -- make space for xmobar
           $ smartBorders  -- no border when only one window
           $ masterStack ||| monocle ||| grid
  where
    masterStack = named masterStackName
                  $ spacingRaw
                    False                 -- smart border
                    (uniGap gapSize) True -- screen gap, enabled
                    (uniGap gapSize) True -- window gap, enabled
                  $ Tall 1 (5/100) (1/2)
    monocle     = named monocleName
                  $ Full
    grid        = named gridName
                  $ spacingRaw
                    False                 -- smart border
                    (uniGap gapSize) True -- screen gap, enabled
                    (uniGap gapSize) True -- window gap, enabled
                  $ Grid

masterStackName = "<fc=" ++ focusColor ++ ">[]=</fc> [ ] [+]"
monocleName     = "[]= <fc=" ++ focusColor ++ ">[ ]</fc> [+]"
gridName        = "[]= [ ] <fc=" ++ focusColor ++ ">[+]</fc>"

-- construct a uniform Gap ('Border')
uniGap :: Integer -> Border
uniGap i = Border i i i i


-------------------------------------------------------------------
-- Log Hook
-------------------------------------------------------------------

logHook' xmobarproc screen = marshallPP screen xmobarPP
                      { ppOutput = hPutStrLn xmobarproc
                      , ppSep    = "  ::  "
                      , ppCurrent = color focusColor
                      , ppHidden = color "gray"
                      , ppHiddenNoWindows = const "_"
                      , ppTitle  = shorten 50
                      , ppVisible = color "white"
                      , ppVisibleNoWindows = Just $ color "white" . const "-"
                      }
                      where color c = xmobarColor c ""


 -------------------------------------------------------------------
 -- Helper Functions
 -------------------------------------------------------------------

--
-- starting xmobar
--
xmobarCommand (S s) = unwords ["xmobar -x", show s, template s]
  where
    template 0 = ""
    template _ = "-t %StdinReader%"

