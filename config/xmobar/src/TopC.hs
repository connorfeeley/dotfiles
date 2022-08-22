import Xmobar
import Config
import Monitors
import Music (mpris, gpmd, mpdt)

memoratio = Memory ["-t","<usedratio>%", "-p", "2", "-W", "3"] 20

topProcL p = TopProc (p <~> ["-t"
                            , memTemp
                              ++ "  ·  <mboth1>  <mboth2>  <mboth3>  <mboth4>"
                            , "-w", "12", "-L" , "10", "-H", "80"]) 15
             where memTemp = if pIsLight p
                             then "<both1>  <both2>  <both3>  <both4>"
                             else "<both1> <both2> <both3>"

diskIOL p = DiskIO [("/", "<totalbipat>"), ("/home", "<totalbipat>")]
                   (diskArgs p) 10

diskIOS p = DiskIO [("/", "<read> <write>")] (diskArgs p) 10

mpd a p i =
  MPDX [ "-W", "12", "-b", "░", "-f", "▒", "-t", "<statei> <remaining>"
       , "--", "-p", p, "-P", fni "\xf144", "-Z", fni i, "-S", fni i] 20 a

mpdMon = mpd "mpd" "6600" "\xf001"
mopMon = mpd "mopidy" "6669" "\xf1bc"

mail' p = MailX [ ("J", "jao.inbox", pHigh p)
                , ("H", "jao.hacking", "")
                , ("b", "bigml.bugs", pHigh p)
                , ("B", "bigml.inbox", "")
                , ("S", "bigml.support", "")
                -- , ("W", "bigml.drivel", pLow p)
                -- , ("l", "bigml.lists", pLow p)
                -- , ("D", "jao.drivel", pLow p)
                -- , ("F", "feeds", pLow p)
                -- , ("E", "feeds.emacs", pLow p)
                -- , ("P", "feeds.prog", pLow p)
                -- , ("w", "feeds.words", pLow p)
                ]
          [ "-d", "~/var/mail" , "-s", " "] "mail"

mail'' p = MailX [] [ "-d", "~/var/mail" , "-s", " "] "mail"

config p = (baseConfig p) {
  position = TopSize C 100 (defaultHeight - 1)
  , font = "xft:Roboto Mono-8"
  , textOffset = defaultHeight - 8
  , textOffsets = [defaultHeight - 9, defaultHeight - 9,
                   defaultHeight - 6, defaultHeight - 8,
                   defaultHeight - 8, defaultHeight - 8]
  , alpha = 192
  , border = FullB
  , commands = [ Run (topProcL p)
               , Run (iconBatt p)
               , Run mpdMon
               , Run (NamedXPropertyLog "_EMACS_LOG" "elog")
               , Run (cpu p)
               , Run memory
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
               ] ++ trayC
  , template = trayT
             ++ " |batt0|" ++ sep
             ++ dimi "\xf26c" ++ "  |bright|" ++ sep
             ++ "<action=`toggle-app.sh nm-applet`>"
             ++ " |wg-mullvad||tun0||wlp164s0wi|"
             ++ "</action>"
             ++ " |dynnetwork| " ++ sep
             ++ "<action=`toggle-app.sh pasystray`>"
             ++ "  |default:Master| " ++ dimi "\xf130" ++ " |default:Capture|"
             ++ "</action>" ++ sep
             ++  "|mpd|" ++ sep
             ++ " |EGPH| "
             ++ fc (pHigh p) "|elog|"
             ++ " {} "
             ++ "|kbd|" ++ sep
--             ++ "|mail|" ++ sep
             ++ "|load| " ++ sep
             ++ "|multicpu| "
             ++ "|multicoretemp| " ++ sep
             ++ " |top| " ++ sep
             ++ " " ++ fni "\xf0c9" ++ " |memory| "
             ++ " |diskio| |disku| " ++ sep
             ++ "  |datetime| "
             ++ "|laTime| "
  } where dimi = fc (pDim p) . fni
          sep = "  "
          w = weather' "<skyConditionS> <tempC>° <weather>" "EGPH" p
          isXmonad = pWm p == Just "xmonad"
          trayT = if isXmonad then "|tray|" else ""
          trayC = if isXmonad
                  then [Run (NamedXPropertyLog "_XMONAD_TRAYPAD" "tray")]
                  else []

main :: IO ()
main = palette >>= configFromArgs . config >>= xmobar
