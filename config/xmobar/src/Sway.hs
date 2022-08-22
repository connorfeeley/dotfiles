import Xmobar
import Config
import Monitors

memoratio = Memory ["-t","<usedratio>%", "-p", "2", "-W", "3"] 20

topProcL p = TopProc (p <~> ["-t"
                            , memTemp
                              ++ "  ·  <mboth1>  <mboth2>  <mboth3>  <mboth4>"
                            , "-w", "12", "-L" , "10", "-H", "80"]) 15
             where memTemp = if pIsLight p
                             then "<both1>  <both2>  <both3>  <both4>"
                             else "<both1>  <both2>  <both3>"

diskIOS p = DiskIO [("/", "<total>"), ("/home", "<total>")] (diskArgs p) 10

mpd a p i = MPDX [ "-W", "12", "-t", "<statei> <remaining>"
                 , "--", "-p", p, "-P", "\xf144", "-Z",fni i, "-S", fni i] 20 a

mpdMon = mpd "mpd" "6600" "🎶"

mprisx client width =
  Mpris2 client ["-t" , " <title> "
                , "-T", show width
                , "-E", "…", "-M", "100", "-x", ""] 40


config p = (baseConfig p) {
  position = TopSize C 100 (defaultHeight - 1)
  -- , textOutput = True
  -- , textOutputFormat = Swaybar
  , font = "Source Code Pro Medium 9"
  , additionalFonts = []
  , bgColor = "#ffffffc0"
  , fgColor = "#000000"
  , border = FullB
  , commands = [ Run (topProcL p)
               , Run (iconBatt p)
               , Run mpdMon
               , if pIsLight p then Run (cpu p) else  Run (cpuBars p)
               , Run memoratio
               , Run (diskU p)
               , Run (diskIOS p)
               , Run brightness
               , Run (kbd p)
               , Run (coreTemp p)
               , Run (wireless p "wlp164s0")
               , Run (dynNetwork p)
               , Run (vpnMark "wg-mullvad")
               , Run tun0
               , Run (masterVol p)
               , Run captureVol
               , Run laTime
               , Run localTime
               , Run w -- LEGE, LEBL, KCV0
--               , Run (mprisx "playerctld" 20)
               ]
  , template = "" -- " |mpris2| "
             ++ " |batt0| "
             ++ dimi "\xf26c" ++ " |bright| "
             ++ "<action=`toggle-app.sh nm-applet --indicator`>"
             ++ " |wg-mullvad||tun0||wlp164s0wi|"
             ++ "</action>"
             ++ " |dynnetwork| "
             ++ "<action=`toggle-app.sh pavucontrol`>"
             ++ "  |default:Master| " ++ dimi "\xf130" ++ " |default:Capture|"
             ++ "</action>  "
             ++  "|mpd|"
             ++ " |EGPH| "
             ++ " {} <hspace=3/>"
             ++ "|multicpu| "
             ++ "|multicoretemp| "
             ++ " |top| "
             ++ " " ++ "☸" ++ " |memory| "
             ++ " |diskio| |disku| "
             ++ " |datetime| "
             ++ " |laTime|  "
  } where dimi = fc (pDim p) . fni
          w = weather' "<skyConditionS> <tempC>° <weather>" "EGPH" p

main :: IO ()
main = palette >>= configFromArgs . config >>= xmobar
