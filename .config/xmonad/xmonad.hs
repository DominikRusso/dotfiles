import Data.List (find)
import System.Exit
import XMonad
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.Run

import qualified Data.Map        as M
import qualified XMonad.StackSet as W


-------------------------------------------------------------------
-- Main
-------------------------------------------------------------------

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0"
  xmonad $ docks def
    {
      borderWidth        = borderWidth'
    , focusedBorderColor = focusedBorderColor'
    , keys               = keys'
    , layoutHook         = layouts'
    , logHook            = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn xmproc0
                            , ppSep    = "  ::  "
                            , ppCurrent = xmobarColor "yellow" ""
                            , ppHidden = xmobarColor "gray" ""
                            , ppHiddenNoWindows = const "_"
                            , ppTitle  = shorten 80
                            }
    , modMask            = modMask'
    , normalBorderColor  = normalBorderColor'
    , terminal           = terminal'
    , workspaces         = workspaces'
    }

terminal'           = "alacritty"

modMask'            = mod4Mask -- super key
workspaces'         = map show $ [1..9] ++ [0]

borderWidth' = 1
focusedBorderColor' = "yellow"
normalBorderColor'  = "black"


-------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------

keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [
  -- navigating
    ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_m), windows W.focusMaster)

  -- moving windows
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm .|. shiftMask, xK_m), windows W.swapMaster)
  , ((modm,               xK_t), withFocused $ windows . W.sink) -- (t)ile floating window

  -- controlling gaps
  , ((modm .|. shiftMask, xK_g), decScreenWindowSpacing 2)
  , ((modm,               xK_g), incScreenWindowSpacing 2)
  , ((modm .|. mod1Mask,  xK_g), sequence_ [toggleScreenSpacingEnabled,
                                            toggleWindowSpacingEnabled])

  -- layouts
  , ((modm .|. shiftMask, xK_Tab  ), sendMessage FirstLayout)
  , ((modm .|. shiftMask, xK_space), sendMessage FirstLayout)
  , ((modm,               xK_Tab  ), sendMessage NextLayout)
  , ((modm,               xK_space), sendMessage NextLayout)
  , ((modm,               xK_b    ), bindOn LN [(monocleName, sendMessage ToggleStruts)])


  -- opening and closing programs
  , ((modm .|. shiftMask,   xK_Escape), io (exitWith ExitSuccess))
  , ((modm .|. controlMask, xK_r     ), spawn "xmonad --recompile && xmonad --restart")
  , ((modm .|. shiftMask,   xK_q     ), kill)
  , ((modm,                 xK_Return), spawn "alacritty")
  , ((modm,                 xK_d     ), spawn "dmenu_run")
  , ((modm,                 xK_o     ), submap . M.fromList $
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
  , ((modm, xK_x), spawn "physlock")
  , ((modm, xK_s), submap . M.fromList $
      -- (s)ystem
      [ ((0, xK_h), spawn "dmenu_prompt \"Hibernate?\" \"systemctl hibernate\"")
      , ((0, xK_r), spawn "dmenu_prompt \"Reboot?\" \"sudo -A reboot\"")
      , ((0, xK_s), spawn "dmenu_prompt \"Shutdown?\" \"sudo -A shutdown -h now\"")
      , ((0, xK_z), spawn "systemctl suspend")
      ])

  ]

  ++

  -- mod-[1..9, 0]       switch to workspace N
  -- mod-shift-[1..9, 0] move window to workspace N
  [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]


-------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------

layouts' = avoidStruts   -- make space for xmobar
         $ smartBorders  -- no border when only one window
         $ masterStack ||| monocle ||| grid
  where
    masterStack = named masterStackName $ spacingRaw False (uniBorder 6) True (uniBorder 6) True $ Tall 1 (5/100) (1/2)
    monocle     = named monocleName $ Full
    grid        = named gridName $ spacingRaw False (uniBorder 6) True (uniBorder 6) True $ Grid

masterStackName = "<fc=yellow>[]=</fc> [ ] [+]"
monocleName     = "[]= <fc=yellow>[ ]</fc> [+]"
gridName        = "[]= [ ] <fc=yellow>[+]</fc>"

-- construct a uniform 'Border'
uniBorder :: Integer -> Border
uniBorder i = Border i i i i


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

