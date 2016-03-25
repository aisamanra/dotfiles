{-# LANGUAGE TupleSections #-}

import           Control.Monad (when, void)
import qualified Data.Map as M
import           Data.Maybe (maybe)
import           System.Directory (doesFileExist)
import           System.IO (hPutStrLn)
import           System.Posix.Env (getEnv)
import           System.Process (createProcess, proc)

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import           XMonad.Layout.NoBorders (noBorders, smartBorders)
import           XMonad.Layout.Tabbed
import           XMonad.Util.Run(spawnPipe)

data ColorScheme = ColorScheme
  { normalColor  :: String
  , focusedColor :: String
  }

grayScheme = ColorScheme "#dddddd" "#999999"
white = "#ffffff"
black = "#000000"

myKeys (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((modMask, xK_p), spawn "dmenu_run")
  ]

myLayout = avoidStruts (myTabbed ||| tiled ||| Mirror tiled) ||| noBorders (smartBorders Full)
  where tiled    = Tall nmaster delta ratio
        nmaster  = 1
        ratio    = 1/2
        delta    = 3/100
        myTabbed = tabbed shrinkText defaultTheme
                     { activeColor         = focusedColor grayScheme
                     , inactiveColor       = normalColor grayScheme
                     , activeBorderColor   = black
                     , inactiveBorderColor = black
                     , activeTextColor     = white
                     , inactiveTextColor   = white
                     }

workspaceOnScreen :: (WorkspaceId -> WindowSet -> WindowSet) ->
                     WorkspaceId -> X ()
workspaceOnScreen f w =
  maybe (return ()) check (lookup w wsMap)
    where check s = do
            mws <- screenWorkspace s
            case mws of
              Nothing -> windows (f w)
              Just ws -> do
                windows (f ws)
                windows (f w)

onScreen :: ScreenId -> [String] -> [(String, ScreenId)]
onScreen i ws = map (,i) ws

wsMap :: [(String, ScreenId)]
wsMap = (onScreen 0 $ words "1 2 3 4 5") ++
        (onScreen 1 $ words "6 7 8 9")

main :: IO ()
main = do
  fehBgExists <- doesFileExist "/home/gdritter/.xm-init"
  when fehBgExists $
    void (createProcess (proc "sh" ["/home/gdritter/.xm-init"]))
  xmproc <- spawnPipe "xmobar /home/gdritter/.xmobarrc"
  void (spawnPipe "runsvdir /home/gdritter/.run/service")
  xmonad $ defaultConfig
    { modMask            = mod4Mask
    , terminal           = "urxvt -e tmux"
    , keys               = myKeys <+> keys defaultConfig
    , layoutHook         = myLayout
    , normalBorderColor  = normalColor grayScheme
    , focusedBorderColor = focusedColor grayScheme
    , logHook            = dynamicLogWithPP $ xmobarPP
        { ppOutput  = hPutStrLn xmproc
        , ppTitle   = xmobarColor "#999999" "" . shorten 50
        , ppCurrent = xmobarColor "#999999" "" . ("<" ++) . (++ ">")
        }
    }
