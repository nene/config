import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Hooks.ManageHelpers

main = xmonad $ kdeConfig
 
 { modMask = mod4Mask -- use the Windows button as mod
 , manageHook = manageHook kdeConfig <+> myManageHook
 }
 where
   myManageHook = composeAll . concat $
     [ [ className   =? c --> doFloat           | c <- myFloats]
     , [ title       =? t --> doFloat           | t <- myOtherFloats]
     , [ className   =? c --> doF (W.shift "3") | c <- webApps]
     , [ className   =? c --> doF (W.shift "4") | c <- ircApps]
     , [ composeOne [ isFullscreen -?> doFullFloat ] ]
     ]
   myFloats      = ["MPlayer", "Gimp", "Skype", "Plasma-desktop", "VirtualBox"]
   myOtherFloats = ["alsamixer"]
   webApps       = ["Firefox-bin", "Opera"] -- open on desktop 3
   ircApps       = ["Ksirc"]                -- open on desktop 4
   
