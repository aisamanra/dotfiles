{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (void, when)
import Data.Default (def)
import qualified Data.Map as M
import qualified Graphics.X11.ExtraTypes.XF86 as X11
import qualified System.Directory as Sys
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.Process as Sys
import XMonad ((<+>), (|||))
import qualified XMonad as XM
import qualified XMonad.Hooks.DynamicLog as Log
import qualified XMonad.Hooks.ManageDocks as XM
import qualified XMonad.Hooks.SetWMName as XM
import qualified XMonad.Layout.NoBorders as XM
import qualified XMonad.Layout.Tabbed as Tab
import qualified XMonad.Util.Paste as Paste
import qualified XMonad.Util.Run as Run

-- | A 'ColorScheme' represents a handful of salient colors used in
--   the configuration.
data ColorScheme = ColorScheme
  { normalC :: String,
    focusedC :: String,
    blackC :: String,
    grayC :: String,
    whiteC :: String
  }
  deriving (Eq, Show, Read)

-- | Here's a reasonable default color scheme with some blues!
blueScheme :: ColorScheme
blueScheme =
  ColorScheme
    { normalC = "#336699",
      focusedC = "#9ebedf",
      blackC = "#ffffff",
      grayC = "#999999",
      whiteC = "#000000"
    }

keys :: XM.XConfig XM.Layout -> M.Map (XM.ButtonMask, XM.KeySym) (XM.X ())
keys (XM.XConfig {XM.modMask = mdMask}) =
  M.fromList
    [ ((mdMask, XM.xK_p), XM.spawn "dmenu_run"),
      ((mdMask, XM.xK_o), XM.spawn "dmesktop"),
      ((mdMask, XM.xK_period), XM.spawn "ibus engine xkb:us::eng"),
      ((mdMask, XM.xK_u), XM.spawn "amixer -q sset Master 3%+"),
      ((mdMask, XM.xK_d), XM.spawn "amixer -q sset Master 3%-"),
      ((mdMask, XM.xK_m), XM.spawn "amixer -q sset Master 0%"),
      ((mdMask, XM.xK_c), xCopy),
      ((mdMask, XM.xK_v), Paste.pasteSelection),
      ((mdMask, 0x1008ff13), XM.spawn "amixer -q set Master 3%+"),
      ((mdMask, 0x1008ff12), XM.spawn "amixer set Master toggle"),
      ((mdMask, 0x1008ff11), XM.spawn "amixer -q set Master 3%-"),
      ((mdMask, 0x1008ff14), XM.spawn "mpc toggle"),
      ((mdMask, 0x1008ff15), XM.spawn "mpc stop"),
      ((mdMask, 0x1008ff16), XM.spawn "mpc prev"),
      ((mdMask, 0x1008ff17), XM.spawn "mpc next")
    ]

xCopy :: XM.X ()
xCopy = XM.withFocused $ \w -> do
  winName <- XM.runQuery XM.className w
  if winName == "Alacritty"
    then (Paste.sendKey XM.noModMask X11.xF86XK_Copy)
    else (Paste.sendKey XM.controlMask XM.xK_c)

recompile :: IO ()
recompile = do
  putStrLn "recompiling with new-build"
  let cmd = (Sys.proc "cabal" ["new-build"]) {Sys.cwd = Just "/home/gdritter/.xmonad"}
  (code, stdout, stderr) <-
    Sys.readCreateProcessWithExitCode cmd ""
  putStr stdout
  putStr stderr
  putStrLn "Done!"
  Sys.exitWith code

-- This is just out of programmer laziness: a typical XMonad config
-- has a pretty huge type parameter representing the possible layouts.
-- This just wraps an existential around the configuration so that
-- we don't have to write it at the top-level!
data XMConfig
  = forall l.
    ( XM.LayoutClass l XM.Window,
      Read (l XM.Window)
    ) =>
    XMConfig (XM.XConfig l)

-- This builds a config after being given a handle to the xmobar process
-- as well as a color scheme to use.
config :: Sys.Handle -> ColorScheme -> XMConfig
config xmproc ColorScheme {..} = XMConfig conf
  where
    conf =
      def
        { XM.modMask = XM.mod4Mask,
          XM.terminal = "alacritty -e tmux",
          XM.keys = keys <+> XM.keys def,
          XM.handleEventHook =
            XM.docksEventHook <+> XM.handleEventHook def,
          XM.startupHook =
            XM.setWMName "LG3D" <+> XM.docksStartupHook <+> XM.startupHook def,
          XM.layoutHook =
            XM.avoidStruts tiled
              ||| XM.avoidStruts (XM.Mirror tiled)
              ||| XM.noBorders (XM.smartBorders XM.Full)
              ||| XM.avoidStruts tabbed,
          XM.manageHook =
            XM.manageDocks <+> XM.manageHook def,
          XM.normalBorderColor = normalC,
          XM.focusedBorderColor = focusedC,
          XM.logHook =
            Log.dynamicLogWithPP $
              def
                { Log.ppOutput = Sys.hPutStrLn xmproc
                }
        }
    tiled = XM.Tall 1 (3 / 100) (3 / 5)
    tabbed =
      Tab.tabbedAlways
        Tab.shrinkText
        def
          { Tab.activeColor = focusedC,
            Tab.inactiveColor = normalC,
            Tab.activeBorderColor = blackC,
            Tab.inactiveBorderColor = blackC,
            Tab.activeTextColor = whiteC,
            Tab.inactiveTextColor = whiteC
          }

main :: IO ()
main = do
  -- Here we're going to intercept the arguments before xmonad can so we
  -- can hook our own recompile step
  args <- Sys.getArgs
  when ("--recompile" `elem` args) recompile

  -- The .xm-init file gets run if it exists to do setup of the X11
  -- environment.
  xmInitExists <- Sys.doesFileExist "/home/gdritter/.xm-init"
  when xmInitExists $
    void (Sys.createProcess (Sys.proc "sh" ["/home/gdritter/.xm-init"]))

  -- Run an xmobar instance
  xmproc <- Run.spawnPipe "/home/gdritter/.cargo/bin/knurling"

  -- Finally, build the config and run xmonad!
  case config xmproc blueScheme of
    XMConfig c -> XM.xmonad c
