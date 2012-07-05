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
myManageHooks :: [ManageHook]
myManageHooks =
  [ resource =? "Do" --> doIgnore
  , isFullscreen --> doFullFloat
  , className =? "Chromium-browser" --> viewShift "1:web"
  --, className =? "Gnome-terminal" --> doShift "2:term"
  , appName   =? "vim" --> viewShift "3:vim"
  , className =? "Evince" --> viewShift "4:read"
  , className =? "MendeleyDesktop" --> viewShift "4:read"
  , className =? "psi" --> viewShift "5:chat"
  , className =? "Gajim" --> viewShift "5:chat"
  , className =? "Skype" --> viewShift "5:chat"
  , className =? "Balsa" --> viewShift "6:mail"
  , appName   =? "mutt" --> viewShift "6:mail"
  , className =? "Rhythmbox" --> viewShift "7:music"
  , className =? "Spotify" --> viewShift "7:music"
  , className =? "Gmpc" --> viewShift "7:music"
  , className =? "Vlc" --> viewShift "8:video"
  , manageDocks
  ]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

myLayout = chatLayout $ videoLayout $ defaultLayout
  where
    chatLayout = onWorkspace "5:chat" (avoidStruts $ withIM (1%7) chatWindows (Grid ||| simpleTabbed))
    videoLayout = onWorkspace "8:video" (smartBorders Full)
    defaultLayout = avoidStruts tiled ||| avoidStruts (Mirror tiled) ||| avoidStruts Grid ||| avoidStruts simpleTabbed ||| smartBorders Full
    chatWindows = (ClassName "Skype" `And` Resource "skype") `Or` (ClassName "psi" `And` Resource "main") `Or` (ClassName "Gajim" `And` Role "roster")
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 2/3
    delta   = 3/100

main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/tim/.xmobarrc"
  xmonad $ defaultConfig {
    terminal = "urxvt"
    , workspaces = ["1:web", "2:term", "3:vim", "4:read", "5:chat", "6:mail", "7:music", "8:video", "9", "0"]
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

