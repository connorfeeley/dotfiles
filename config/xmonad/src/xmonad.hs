import Config (defaultHeight, pIsLight, pHigh, fc, fni)
import GridSelection

import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Column
import XMonad.Layout.Decoration
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Simplest
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.Replace (replace)
import Control.Monad (when)
import System.Environment (getArgs)
import XMonad.Util.Run
import qualified XMonad.Util.Hacks as Hacks

-- Scratchpads
import XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

-- The main function.
main = do
  args <- getArgs
  when ("--replace" `elem` args) replace
  xmonad $ withUrgencyHook NoUrgencyHook $ withSB (xmobarTop <> xmobarBottom) myConfig

-- Command to launch the bar.
xmobarTop = statusBarPropTo "_XMONAD_LOG_TOP" "xmobar-top" (pure barPP)
xmobarBottom = statusBarPropTo "_XMONAD_LOG_BOTTOM" "xmobar-bottom" (pure barPP)

-- <colors>
colorBlack = "#000000"
colorBlackAlt = "#040404"
colorGray = "#444444"
colorGrayAlt = "#282828"
colorDarkGray = "#161616"
colorWhite = "#cfbfad"
colorWhiteAlt = "#8c8b8e"
colorDarkWhite = "#606060"
colorCream = "#a9a6af"
colorDarkCream = "#5f656b"
colorMagenta = "#a488d9"
colorMagentaAlt = "#7965ac"
colorDarkMagenta = "#8e82a2"
colorBlue = "#98a7b6"
colorBlueAlt = "#598691"
colorDarkBlue = "#464a4a"
colorNormalBorder = colorMagenta
colorFocusedBorder = colorMagenta

-- <font>
barFont = "xft:Iosevka Extended:pixelsize=64:bold:antialias=true"

-- <tab-bar configuration>
myTabTheme =
    def
        { fontName = barFont
        , inactiveBorderColor = colorGrayAlt
        , inactiveColor = colorDarkGray
        , inactiveTextColor = colorGrayAlt
        , activeBorderColor = colorGrayAlt
        , activeColor = colorDarkMagenta
        , activeTextColor = colorDarkGray
        , urgentBorderColor = colorBlackAlt
        , urgentTextColor = colorWhite
        , decoHeight = defaultHeight
        }

myLayoutHook =
    avoidStruts $
        smartBorders $
            mkToggle (single REFLECTX) $
                mkToggle (single REFLECTY)
                    ( tall
                        ||| tabs
                        ||| threeHalf
                        ||| threeThird
                        ||| Column (1 / 2)
                        ||| ThreeCol 1 (3 / 100) (1 / 2)
                    )
  where
    makeTabs l =
        tabBar shrinkText myTabTheme Top $
            resizeVertical (fi $ decoHeight myTabTheme)
                l
    tall = Tall nmaster delta ratio
    tabs = renamed [Replace "Tabs"] $ makeTabs Full
    threeHalf = renamed [Replace "Half"] $ ThreeColMid 1 delta (1 / 2)
    threeThird = renamed [Replace "Third"] $ makeTabs $ ThreeColMid 1 delta (1 / 3)
    nmaster = 1
    delta = 3 / 100
    ratio = 2 / 3

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig{XMonad.modMask = modMask} = (modMask, xK_b)

myManageHook =
    composeAll
        [ isFullscreen --> doFullFloat
        , --   , className =? "Microsoft Teams Notification" --> doFloat
          --   , className =? "microsoft teams - preview" --> doFloat
          className =? "Microsoft Teams - Preview" --> doFloat
        , resource =? "Microsoft Teams Notification" --> doFloat
        , title =? "Extension: (Tree Style Tab) - Close tabs? — Mozilla Firefox" --> doFloat
        ]

-- Status bar pretty-printer configuration
barPP =
    xmobarPP
        { ppTitle = xmobarColor "chocolate" "" . shorten 50
        , ppCurrent = xmobarColor "chocolate" "" . wrap "<" ">"
        , ppVisible = xmobarColor "cornflowerblue" "" . wrap "[" "]"
        , ppHidden = xmobarColor "lightsteelblue" "" . wrap "|" "|"
        , ppUrgent = xmobarColor "red" "" . wrap "<!" "!>"
        , ppWsSep = " • "
        , ppLayout =
            \layout -> case layout of
              "Tall" -> "[|]"
              "Tabs" -> "[*]"
              "Half" -> "[-]"
              "Third" -> "[▯]"
              "Full" -> "[ ]"
              "Column 0.5" -> "[/]"
              "ThreeCol" -> "[||]"
        }

myLogHook pipe =
    workspaceNamesPP
        barPP
            { ppOutput = hPutStrLn pipe
            }
        >>= dynamicLogWithPP

xpConfig :: XPConfig
xpConfig =
    def
        { font = barFont
        , bgColor = "black"
        , fgColor = "grey"
        , promptBorderWidth = 0
        , position = Top
        , height = defaultHeight
        , historySize = 256
        }

scratchpads =
  [NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    findEmacsScratch = title =? "emacs-everywhere"
    spawnEmacsScratch =
      "emacsclient -nc --eval '(emacs-everywhere)' --frame-parameters='(quote (name . \"emacs-scratch\"))'"
    manageEmacsScratch = nonFloating

-- Main configuration
myConfig =
    docks def
        { modMask = mod4Mask
        , layoutHook = myLayoutHook
        , startupHook = setWMName "LG3D"
        -- , handleEventHook = mconcat [handleEventHook (Hacks.windowedFullscreenFixEventHook desktopConfig]
        -- , handleEventHook = Hacks.windowedFullscreenFixEventHook <+> handleEventHook def
        , handleEventHook = handleEventHook def <> Hacks.windowedFullscreenFixEventHook
        , manageHook = manageHook desktopConfig <+> myManageHook <+> namedScratchpadManageHook scratchpads
        , terminal = "kitty"
        , XMonad.workspaces =
            [ "1:web"
            , "2:term"
            , "3:src"
            , "4"
            , "5"
            , "6"
            , "7:teams"
            , "8:vm"
            , "9:org"
            ]
        }
        `additionalKeys` [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
                         , ((mod4Mask .|. controlMask, xK_Return), spawn "alacritty -e $SHELL -c 'ssh -o \"Compression no\" -o \"RequestTTY yes\" -Y cfeeley@192.168.122.180; $SHELL'")
                         , -- , ((mod4Mask .|. controlMask, xK_Return)       , spawn "alacritty -e $SHELL -c 'ssh -o \"Compression no\" -o \"RequestTTY yes\" -Y cfeeley@192.168.122.180 tmux'")
                           ((mod4Mask .|. shiftMask .|. controlMask, xK_Return), spawn "ssh work ~/.scripts/emacsterm &!")
                         , ((0, xK_Super_L), return ()) -- Capture mod key so that it isn't passed to VirtualBox
                         , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +5%")
                         , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 5%-")
                         , -- Flip layout on Y axis
                           ((mod4Mask, xK_f), sendMessage $ Toggle REFLECTX)
                         , -- Hotkeys for Model M keyboard, which doesn't have a fn key
                           ((mod4Mask, xK_F1), spawn ".xmonad/volume \"-5%\"")
                         , ((mod4Mask, xK_F2), spawn ".xmonad/volume \"+5%\"")
                         , ((mod4Mask, xK_F3), spawn ".xmonad/volume \"0%\"")
                         , -- Hotkeys for das keyboard, which does have a fn key
                           ((0, xF86XK_AudioLowerVolume), spawn "~/.xmonad/volume \"-5\"")
                         , ((0, xF86XK_AudioRaiseVolume), spawn "~/.xmonad/volume \"5\"")
                         , ((0, xF86XK_AudioMute), spawn "~/.xmonad/volume \"0\"")
                         , ((0, xF86XK_Display), spawn "xrandr --output eDP-1 --auto --output DP-1-2 --auto --right-of eDP-1 --scale 1x1 --output DP-2-1 --auto --right-of DP-1-2")
                         , ((0, xF86XK_AudioStop), spawn "doom everywhere")
                         , ((0, xK_F9), spawn "PAUSED=$(curl -s localhost:4000/status | jq --raw-output '.Pause'); curl -s localhost:4000/pause; if [ $PAUSED = 'true' ]; then notify-send 'Enabled RBM'; else notify-send 'Paused RBM'; fi")
                         , --
                           -- Other hotkeys
                           --
                           -- Replace dmenu with yeganesh (wrapper around dmenu)
                           ((mod4Mask, xK_p), spawn "x=$(yeganesh -x -- -i -fn Terminus-32) && exec $x")
                         , -- Execute on VM
                           ((mod4Mask .|. controlMask, xK_p), spawn "x=$(ssh -X work $(yeganesh -x -- -i -fn Terminus-32)) && exec ssh -X work $x")
                         , -- , ((mod4Mask .|. shiftMask, xK_e), spawn "emacsclient --eval '(emacs-everywhere)'")
                           ((mod4Mask .|. controlMask, xK_e), spawn "emacsclient -e '(emacs-everywhere)'")
                         , ((mod4Mask, xK_n), XMonad.Actions.WorkspaceNames.renameWorkspace def)
                         , ((mod4Mask, xK_b), sendMessage ToggleStruts)
                         , ((mod4Mask, xK_v), selectWorkspace xpConfig)
                         , -- work-container
                           ((mod4Mask .|. controlMask, xK_Return), spawn "alacritty -e $SHELL -c 'ssh -Y cfeeley@work-container; $SHELL'")
                         , ((mod4Mask .|. controlMask, xK_p), spawn "x=$(ssh -X work-container $(yeganesh -x -- -i -fn Terminus-32)) && exec ssh -X work-container $x")

                         , ((mod4Mask .|. shiftMask, xK_period), namedScratchpadAction scratchpads "emacs-scratch")
                         , gsLaunch mod4Mask
                         ]
