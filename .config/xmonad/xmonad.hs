import XMonad

main :: IO ()
main = xmonad defaultConfig
  {
    borderWidth        = borderWidth'
  , focusedBorderColor = focusedBorderColor'
  , modMask            = modMask'
  , normalBorderColor  = normalBorderColor'
  , workspaces         = workspaces'
  }

borderWidth'        = 1
focusedBorderColor' = "#aaaaaa"
modMask'            = mod4Mask
normalBorderColor'  = "#222222"
workspaces'         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
