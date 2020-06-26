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
workspaces'         = map show $ [1..9] ++ [0]

borderWidth' = 1
focusedBorderColor' = "yellow"
normalBorderColor'  = "black"


-------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------

meta   :: KeyMask
ctrl  :: KeyMask
shift :: KeyMask
super :: KeyMask
meta  = mod1Mask
ctrl  = controlMask
shift = shiftMask
super = mod4Mask

-- main modifier key
modMask' = meta

keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [
  -- navigating
    ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_m), windows W.focusMaster)

  -- moving windows
  , ((modm .|. shift, xK_j), windows W.swapDown)
  , ((modm .|. shift, xK_k), windows W.swapUp)
  , ((modm .|. shift, xK_m), windows W.swapMaster)
  , ((modm,           xK_t), withFocused $ windows . W.sink) -- (t)ile floating window

  -- controlling gaps and padding
  , ((modm .|. shift, xK_g), decScreenWindowSpacing 2)
  , ((modm,           xK_g), incScreenWindowSpacing 2)
  , ((modm .|. ctrl,  xK_g), sequence_ [setWindowSpacing defaultGap,
                                        setScreenSpacing defaultGap])
  , ((modm .|. super, xK_g), sequence_ [toggleScreenSpacingEnabled,
                                        toggleWindowSpacingEnabled])
  , ((modm .|. super, xK_b), sendMessage ToggleStruts)
  , ((modm,           xK_f), sequence_ [sendMessage $ JumpToLayout monocleName,
                                        sendMessage $ SetStruts [] [U .. L]])
  -- layouts
  , ((modm .|. shift, xK_Tab  ), sendMessage FirstLayout)
  , ((modm .|. shift, xK_space), sendMessage FirstLayout)
  , ((modm,           xK_Tab  ), sendMessage NextLayout)
  , ((modm,           xK_space), sendMessage NextLayout)

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
  , ((modm          , xK_p), spawn "scrot")
  , ((modm .|. shift, xK_p), spawn "scrot -s")
  , ((modm          , xK_x), spawn "physlock")
  , ((modm          , xK_s), submap . M.fromList $
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
        , (f, m) <- [(W.greedyView, 0), (W.shift, shift)]
  ]


-------------------------------------------------------------------
-- Layouts
-------------------------------------------------------------------

layouts' = avoidStruts   -- make space for xmobar
         $ smartBorders  -- no border when only one window
         $ masterStack ||| monocle ||| grid
  where
    masterStack = named masterStackName $ spacingRaw False defaultGap True defaultGap True $ Tall 1 (5/100) (1/2)
    monocle     = named monocleName $ Full
    grid        = named gridName $ spacingRaw False defaultGap True defaultGap True $ Grid

masterStackName = "<fc=yellow>[]=</fc> [ ] [+]"
monocleName     = "[]= <fc=yellow>[ ]</fc> [+]"
gridName        = "[]= [ ] <fc=yellow>[+]</fc>"

defaultGap = uniGap 4

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

