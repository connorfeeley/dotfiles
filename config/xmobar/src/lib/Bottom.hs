module Bottom (config) where

import           Config
import           Monitors
import           Xmobar

-- ⏱
config cs tpl p =
  (baseConfig
     p) { position = BottomSize C 100 defaultHeight
        , textOffset = defaultHeight - 6
        , textOffsets = [ defaultHeight - 6
                        , defaultHeight - 6
                        , defaultHeight - 4
                        , defaultHeight - 8
                        , defaultHeight - 7]
        , border = TopB
        , template = "|tray| \
            \<action=`toggle-app.sh nm-applet`>|proton0||wlp1s0wi|</action> \
            \ |dynnetwork| \
            \ <action=`toggle-app.sh pasystray`>|default:Master|\
            \ |default:Capture|</action>  <fn=2>🎵</fn>"
            ++ tpl
            ++ " {} |mail|  |EGPH| \
                   \ <fn=2>🗓  </fn>|uptime| <fn=2>🕓 </fn>|datetime| |laTime| "
        , commands = [ Run (uptime p)
                     -- , Run (wireless p "wlp1s0")
                     , Run (dynNetwork p)
                     , Run proton0
                     , Run (weather "EGPH" p) -- LEGE, LEBL, KCV0
                     , Run trayPadding
                     -- , Run (mail p)
                     -- , Run (masterVol p)
                     -- , Run captureVol
                     -- , Run laTime
                     , Run localTime]
            ++ cs
        }
