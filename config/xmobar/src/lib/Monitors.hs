module Monitors where

import Xmobar
import Config
import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import qualified Data.Char as Char
import qualified Text.Printf as Printf

data CombinedMonitor a b = CombinedMonitor a b (String -> String -> String)

instance (Show a, Show b) => Show (CombinedMonitor a b) where
  show (CombinedMonitor a b _) = "Alt (" ++ show a ++ ") (" ++ show b ++ ")"

instance (Read a, Read b) => Read (CombinedMonitor a b) where
  readsPrec _ = undefined

instance (Exec a, Exec b) => Exec (CombinedMonitor a b) where
  alias (CombinedMonitor a b _) = alias a ++ "_" ++ alias b
  rate (CombinedMonitor a b _) = min (rate a) (rate b)
  start (CombinedMonitor a b comb) cb
    = startMonitors a b (\s t -> cb $ comb s t)

startMonitors a b cmb =  do
    sta <- atomically $ newTVar ""
    stb <- atomically $ newTVar ""
    _ <- async $ start a (atomically . writeTVar sta)
    _ <- async $ start b (atomically . writeTVar stb)
    go sta stb
      where go sta' stb' = do
              s <- readTVarIO sta'
              t <- readTVarIO stb'
              cmb s t
              tenthSeconds $ min (rate b) (rate a)
              go sta' stb'

guardedMonitor a p = CombinedMonitor (PipeReader p (alias a ++ "_g")) a f
  where f s t = if null s || head s == '0' then "" else t

altMonitor a b = CombinedMonitor a b (\s t -> if null s then t else s)
concatMonitor sep a b = CombinedMonitor a b (\s t -> s ++ sep ++ t)
toggleMonitor path a = altMonitor (guardedMonitor a path)

topProc p = TopProc (p <~> ["-t" , "<mboth3>  <mboth2>  <mboth1> \
                                   \· <both3>  <both2>  <both1>"
                           , "-w", "10", "-L" , "10", "-H", "80"]) 15

topProc' p = TopProc (p <~> ["-t" , "<mboth1>  <mboth2>  <mboth3> \
                                   \· <both1>  <both2>  <both3>"
                           , "-w", "10", "-L" , "10", "-H", "80"]) 15

-- wireless p n = Wireless n (p >~< ["-t", "<essid>"
--                                  -- fc (pLow p) (fni "\xf1eb " ++ "<essid>")
--                                  -- <quality>\xf09e
--                                  , "-W", "5", "-M", "15" , "-m", "3"
--                                  , "-L", "20", "-H", "80"]) 20

cpu p = MultiCpu (p <~> ["-t", "<total>"
                        , "-S", "on", "-c", " " , "-L", "30", "-H", "70"
                        , "-p", "3", "-a", "l"]) 10

multiCPU p = MultiCpu (p <~> ["-t", "<autototal>"
                             , "-S", "on", "-b", " ", "-f", "*"
                             , "-c", " " , "-L", "30", "-H", "70"
                             , "-p", "3", "-a", "l"]) 10

cpuBars p = MultiCpu (mkArgs p
                             ["--template" , "<autoipat> <total>%"
                             , "-L", "50", "-H", "85", "-w", "3"]
                             ["--fallback-icon-pattern", "<icon=load_%%.xpm/>"
                             , "--contiguous-icons"])
            10

cpuFreq p = CpuFreq (p <~> ["-t" , "<cpu0> <cpu1> <cpu2> <cpu3>"
                           , "-L", "1", "-H", "2", "-S", "Off" , "-d", "2"]) 50

-- ⤒⤊⍐ ⊼ ⇧  ⇩ ⎗ ⎘
dynNetwork p = DynNetwork (p <~> ["-t", fn 1 "↑ " ++ "<tx>  " ++ fn 1 "↓" ++ " <rx>"
                                 , "-L", "20", "-H", "1024000"
                                 , "-m", "5", "-W", "10", "-S", "Off"]) 10

uptime p = Uptime (p <~> [ "-t" , "<days> <hours>", "-m", "3", "-c", "0", "-S"
                         , "On" , "-L", "10", "-H", "100"]) 600

weather' tmp st p =
  WeatherX st
           [ ("", fc (pDim p) "") -- "🧚"
           , ("clear", fn 4 "🌣")
           , ("sunny", fc (pHigh p) $ fn 4 "🌣")
           , ("fair", fn 4 "🌣")
           , ("mostly clear", fn 4 "🌤")
           , ("mostly sunny", fn 4 "🌤")
           , ("partly sunny", fn 3 "⛅")
           , ("obscured", fn 4 "🌁") -- 🌫
           , ("cloudy", fn 3 "☁")
           -- , ("overcast", fn 3 "☁")
           , ("overcast", fn 3 "☁️")
           , ("partly cloudy", fn 3 "⛅")
           -- , ("mostly cloudy", fn 3 "☁")
           , ("mostly cloudy", fn 3 "☁️")
           , ("considerable cloudiness", fn 4 "⛈")
           , ("light rain", fn 4 "🌧")
           , ("rain", fn 4 "⛆")
           , ("ice crystals", fn 3 "❄")
           , ("light snow", fn 3 "🌨")
           , ("snow", fn 3 "❄")
           ]
           (mkArgs p ["-t", tmp , "-L","10", "-H", "25" , "-T", "25", "-E", ".."]
                     ["-w", ""])
           18000

weather = weather' "<skyConditionS> <tempC>° <rh>% <windKmh> (<hour>)"

-- "https://wttr.in?format=" ++ fnn 3 "%c" ++ "+%t+%C+%w++" ++ fnn 1 "%m"
-- , Run (ComX "curl" [wttrURL "Edinburgh"] "" "wttr" 18000)
wttrURL l = "https://wttr.in/" ++ l ++ "?format=" ++ fmt
  where fmt = fnn 2 "+%c+" ++ "+%t+%C+" ++ fn 5 "%w"
        fnn n x = urlEncode ("<fn=" ++ show n ++ ">") ++ x ++ urlEncode "</fn>"
        encode c
          | c == ' ' = "+"
          | Char.isAlphaNum c || c `elem` "-._~" = [c]
          | otherwise = Printf.printf "%%%02X" c
        urlEncode = concatMap encode

batt p =
  BatteryN ["BAT0"]
           ["-t", "<acstatus> <left>"
           , "-S", "Off", "-d", "0", "-m", "3"
           , "-L", "10", "-H", "90", "-p", "3"
           , "--low", pHigh p, "--normal", pNormal p, "--high", pLow p
           , "--"
           , "-P"
           , "-a", "notify-send -u critical 'Battery running out!!!!!!'"
           , "-A", "7"
           , "-i", fn 2 "\9211"
           , "-O", fn 2 " \9211" ++ " <timeleft> <watts>"
           , "-o", fn 2 " 🔋" ++ " <timeleft> <watts>"
           , "-H", "10", "-L", "7"
           , "-h", pHigh p, "-l", pLow p] 50 "batt0"

iconBatt p =
  BatteryN ["BAT0"]
           ["-t", "<acstatus>"
           , "-S", "Off", "-d", "0", "-m", "3"
           , "-L", "10", "-H", "90", "-p", "3"
           , "-W", "0", "-f",
             "\xf244\xf243\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf240"
           , "--low", pHigh p, "--normal", pNormal p, "--high", pLow p
           , "--"
           , "-P"
           , "-a", "notify-send -u critical 'Battery running out!!!!!!'"
           , "-A", "5"
           , "-i", fni "\xf1e6"
           , "-O", fni "<leftbar>  \xf1e6" ++ " <watts> <timeleft>"
           , "-o", fni "<leftbar>" ++ " <watts> <timeleft>"
           , "-H", "10", "-L", "7"
           , "-h", pHigh p, "-l", pLow p] 50 "batt0"

-- rizenTemp p =
--   K10Temp "0000:00:18.3"
--      (mkArgs p ["-t", "<Tctl>°C", "-L", "40", "-H", "70", "-d", "0"] []) 50

thinkTemp p =
  MultiCoreTemp (mkArgs p
                        ["-t", "<core1>°C", "-L", "40", "-H", "70", "-d", "0"]
                        []) 50

avgCoretemp p =
  MultiCoreTemp (p <~> ["-t", "<avg>°"
                       , "-L", "50", "-H", "75", "-d", "0"]) 50

coreTemp p =
  MultiCoreTemp (p <~> ["-t", "<avg>° <max>°"
                       , "-L", "50", "-H", "75", "-d", "0"]) 50

diskU p =
  DiskU [("/", "<used>") , ("/media/sda", " s <used>")]
        (p <~> ["-L", "20", "-H", "70", "-m", "1", "-p", "3"])
        20

diskArgs p = mkArgs p
                ["-f", "░", "-b", " ", "-L", "10000000", "-H" , "100000000"
                , "-W", "5", "-w", "5", "-p", "3"]
                ["--total-icon-pattern", "<icon=load_%%.xpm/>", "-c"]

diskIO p =
  DiskIO [("rivendell-vg/root", "<readb> <writeb> <totalbipat>")] (diskArgs p) 10

-- <fn=1>📨 🖅 🖃 📩 ✉ </fn>
-- (fni "\xf01c \xf03a \xf1fa \xf0e0 \xf1d8 ")
-- mail p = MailX [ ("I", "jao/inbox", pHigh p)
--                , ("b", "bigml/bugs", pHigh p)
--                , ("B", "bigml/inbox", "")
--                , ("S", "bigml/support", "")
--                , ("H", "jao/hacking", "")
--                , ("D", "jao/drivel", "")
--                , ("D", "bigml/drivel", pDim p)
--                , ("R", "feeds/rss", pDim p)
--                , ("E", "feeds/emacs", pDim p)
--                , ("P", "feeds/prog", pDim p)
--                , ("B", "jao/bills", pDim p)
--                , ("L", "bigml/lists", pDim p)
--                ]
--                [ "-d", "~/var/mail"
--                -- , "-p", fc (pHigh p) $ fn 1 "⎘  " -- fc (pLow p) (fni "\xf01c" ++ " ")
--                , "-s", " "
--                ]
--                "mail"

-- masterVol p =
--   Volume "default" "Master"
--                   ["-t", "<status> <volume>"
--                   -- "<status> " ++ fni "<volumebar>" ++ " <volume>"
--                   -- , "-W", "0", "-f", "\xf026\xf026\xf027\xf027\xf028\xf028\xf028"
--                   , "--", "-C", pForeground p, "-c", "sienna4"
--                   -- , "-O", ""
--                   , "-O", fni "\xf025" -- "\xf130" -- fn 2 "🎧"
--                   , "-o", fn 4 "🔇"
--                   ] 10

-- captureVol = Volume "default" "Capture" ["-t", "<volume>"] 10

kbd p = Kbd [("us", ""), ("us(intl)", kbi pHigh)] -- kbi pDim
  where kbi a = fc (a p) (fni " \xf11c")

brightness = Brightness ["--", "-D", "intel_backlight"] 10
brightness' = Brightness ["--", "-D", "amdgpu_bl0", "-C", "brightness"] 10

memory = Memory [ "-t" ,"<used>:<available>"
                , "-p", "2", "-W", "4","-d", "1"
                , "--", "--scale", "1024"] 20

netdev name icon =
  Network name ["-t", "<up>", "-x", "", "--", "--up", icon] 20 -- fn 2 "🔐 "
vpnMark n = netdev n $ fn 2 "🔒 " -- fni "\xf0e8 "
proton0 = vpnMark "proton0"
tun0 = vpnMark "tun0"

-- laTime = DateZone "%H" "en_US" "US/Pacific" "laTime" 10
-- localTime = Date "%a %d %R" "datetime" 10
localTime = Date "<fc=snow>%F (%a) %T</fc>" "date" 10

trayPadding = Com "padding-width.sh" [] "tray" 20
