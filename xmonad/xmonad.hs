import Control.Monad
import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import Data.Ratio ((%))
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run(spawnPipe)
import System.IO
import qualified Data.Map as M


myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList [
 ((modm, xK_p), spawn "/home/tim/.dotfiles/util/dmenu_run")]

myManageHooks :: [ManageHook]
myManageHooks =
  [ isFullscreen --> doFullFloat
  , className =? "Chromium-browser" --> viewShift "1:web"
  , className =? "Evince" --> viewShift "4:read"
  , className =? "MendeleyDesktop" --> viewShift "4:read"
  , className =? "Rhythmbox" --> viewShift "6:music"
  , className =? "Spotify" --> viewShift "6:music"
  , className =? "Gmpc" --> viewShift "6:music"
  , className =? "Vlc" --> viewShift "7"
  , manageDocks
  ]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

myLayout = avoidStruts tiled ||| avoidStruts (Mirror tiled) ||| avoidStruts Grid ||| smartBorders Full
    where tiled   = Tall nmaster delta ratio
          nmaster = 1
          ratio   = 2/3
          delta   = 3/100

main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/tim/.xmobarrc"
  xmonad $ defaultConfig {
    keys = myKeys <+> keys defaultConfig
    , terminal = "urxvt"
    , workspaces = ["1:web", "2:term", "3:vim", "4:read", "5:comms", "6:music", "7", "8", "9", "0"]
    , normalBorderColor = "#002b36"
    , focusedBorderColor = "#839496"
    , borderWidth = 1
    , manageHook = composeAll myManageHooks
    , layoutHook = myLayout
    , logHook    = dynamicLogWithPP xmobarPP {
       ppOutput = hPutStrLn xmproc,
       ppTitle = xmobarColor "#2aa198" "" . shorten 50,
       ppCurrent = xmobarColor "#268bd2" "" . wrap "[" "]",
       ppUrgent = xmobarColor "#dc322f" ""
    }
  }

