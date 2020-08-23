import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Spacing
import qualified Data.Map as M
import XMonad.Util.Loggers
import System.IO

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobarrc"
  xmonad $ desktopConfig
	{ terminal = "urxvt"
        , startupHook = startup
	, modMask = mod4Mask
	, borderWidth = 1
	, layoutHook = avoidStruts $ spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $ layoutHook def
        , logHook =  dynamicLogWithPP cPP { ppOutput = hPutStrLn xmproc0 }
	, manageHook = manageHook def <+> manageDocks
        , keys          = \c -> ckeys c `M.union` keys def c
	}
        where ckeys (XConfig {modMask = modm}) = M.fromList $
                [
                  ((modm , xK_p), spawn "rofi -show combi")
                , ((modm .|. shiftMask , xK_d), spawn "urxvt -e ranger")
                ]

startup :: X ()
startup = do
  spawn "dunst &"
  spawn "compton &"
  spawn "wmname LG3D &"
  spawn "feh --bg-fill --randomize ~/Bilder/Wallpaper/* &"
  spawn "xautolock -time 10 -locker slock -nowlocker slock -detectsleep -corners 000+ -cornerdelay 3 &"

cPP = def
  { ppCurrent = xmobarColor "#1ABC9C" ""
  , ppTitle = xmobarColor "#1ABC9C" "" . shorten 70
  , ppVisible = wrap "(" ")"
  , ppSep = " | "
  , ppLayout = const ""
  , ppUrgent  = xmobarColor "red" "yellow"
  }
