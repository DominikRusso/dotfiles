import Data.List (find)
import System.Exit
import XMonad hiding ( (|||) )
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
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
  xmobar0 <- spawnPipe "xmobar -x 0"
  xmonad $ docks def
    { borderWidth        = borderWidth'
    , focusedBorderColor = focusColor
    , focusFollowsMouse  = focusFollowsMouse'
    , keys               = keys'
    , layoutHook         = layouts'
    , logHook            = logHook' xmobar0
    , modMask            = modMask'
    , normalBorderColor  = normalColor
    , terminal           = terminal'
    , workspaces         = workspaces'
    }


-------------------------------------------------------------------
-- General Config
-------------------------------------------------------------------

focusColor          = "green"
normalColor         = "black"

borderWidth'        = 1
gapSize             = 4

terminal'           = "alacritty"
workspaces'         = map show [1..4]

focusFollowsMouse'  = True
clickJustFocuses    = False


-------------------------------------------------------------------
-- Log Hook
-------------------------------------------------------------------

logHook' xmobarproc = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmobarproc
                        , ppSep    = "  ::  "
                        , ppCurrent = xmobarColor focusColor ""
                        , ppHidden = xmobarColor "gray" ""
                        , ppHiddenNoWindows = const "_"
                        , ppTitle  = shorten 80
                        }


-------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------

ctrl, meta, shift, super :: KeyMask
meta  = mod1Mask
ctrl  = controlMask
shift = shiftMask
super = mod4Mask

-- main modifier key
modMask' = meta

keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [
  -- navigating windows
    ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_m), windows W.focusMaster)

  -- moving windows
  , ((modm .|. shift, xK_j), windows W.swapDown)
  , ((modm .|. shift, xK_k), windows W.swapUp)
  , ((modm .|. shift, xK_m), windows W.swapMaster)
  , ((modm,           xK_h), sendMessage Shrink)
  , ((modm,           xK_l), sendMessage Expand)
  , ((modm,           xK_t), withFocused $ windows . W.sink) -- (t)ile floating window

  -- controlling gaps and padding
  , ((modm,           xK_g), sequence_ [toggleScreenSpacingEnabled,  -- (g)aps
                                        toggleWindowSpacingEnabled])
  , ((modm,           xK_b), sendMessage ToggleStruts)               -- (b)ar

  -- layouts
  , ((modm,           xK_Tab  ), sendMessage NextLayout)

  -- opening and closing programs
  , ((modm .|. ctrl,  xK_Escape), io (exitWith ExitSuccess))
  , ((modm .|. ctrl,  xK_r     ), spawn "xmonad --recompile && xmonad --restart")
  , ((modm .|. shift, xK_q     ), kill)
  , ((modm,           xK_Return), spawn "alacritty")
  , ((modm,           xK_d     ), spawn "dmenu_run")
  , ((modm,           xK_o     ), submap . M.fromList $
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
  , ((modm          , xK_p), spawn "scrot ~/media/images/screenshots/screenshot-%Y-%m-%d-%T.png")
  , ((modm .|. shift, xK_p), spawn "scrot -s ~/media/images/screenshots/screenshot-%Y-%m-%d-%T.png")
  , ((modm          , xK_x), spawn "physlock -m -s")
  , ((modm          , xK_s), submap . M.fromList $
      -- (s)ystem
      [ ((0, xK_h), spawn "dmenu_prompt \"Hibernate?\" \"systemctl hibernate\"") -- suspend to disk
      , ((0, xK_r), spawn "dmenu_prompt \"Reboot?\" \"sudo -A reboot\"")
      , ((0, xK_s), spawn "dmenu_prompt \"Shutdown?\" \"sudo -A shutdown -h now\"")
      , ((0, xK_z), spawn "systemctl suspend") -- suspend to swap (z)zz
      ])
  ]

  ++

  -- mod-[1..k]       switch to workspace N
  -- mod-shift-[1..k] move window to workspace N
  [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1..xK_4]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shift)]
  ]


-------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------

layouts' = avoidStruts   -- make space for xmobar
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
-- Helper Functions
-------------------------------------------------------------------

-- bind keys on a per layout basis (adapted from Ethan Schoonover)
data BindType = WS | LN -- workspace | layout name

chooseAction :: BindType -> (String->X()) -> X()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LN f = withWindowSet (f . description . W.layout . W.workspace . W.current)

-- If current workspace or layout string is listed, run the associated action.
-- If it isn't listed, run the default action (marked with empty string, ""),
-- or do nothing if a default isn't supplied.
-- Note that only the first match counts!
bindOn :: BindType -> [(String, X())] -> X()
bindOn bt bindings = chooseAction bt $ chooser where
    chooser bt = case find ((bt==).fst) bindings of
        Just (_, action) -> action
        Nothing -> case find ((""==).fst) bindings of
            Just (_, action) -> action
            Nothing -> return ()

