import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Config.Desktop (desktopLayoutModifiers)

main = xmonad $ kde4Config
 { modMask = mod4Mask -- use the Windows button as mod
 , manageHook = manageHook kde4Config <+> myManageHook
 , layoutHook = layoutHook kde4Config ||| noBorders Full
 }
 where
   myManageHook = composeAll . concat $
     [ [ className   =? c --> doFloat           | c <- myFloats]
     , [ title       =? t --> doFloat           | t <- myOtherFloats]
     , [ composeOne [ isFullscreen -?> doFullFloat ] ]
     ]
   myFloats      = ["MPlayer", "Gimp", "Skype", "Plasma-desktop", "VirtualBox"]
   myOtherFloats = ["alsamixer"]

