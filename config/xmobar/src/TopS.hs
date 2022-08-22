import           Config
import           Monitors
import           Xmobar
import           System.Environment (getArgs)

config p =
  (baseConfig
     p) { position = TopSize L 100 60
        , sepChar = "&" -- delineator between plugin names and straight text
        , alignSep = "}{" -- separator between left-right alignment
        , template = "&enp38s0& } &multicoretemp& | &multicpu& | &memory& { "
        , commands =
            [ Run $ XPropertyLog "_XMONAD_LOG_TOP"
              -- free disk space monitor
            , Run
              $ Network
                "enp38s0"
                (barTheme
                   [ "--template"
                   , "TX: <tx> KB/s| RX: <rx> KB/s"
                   , "--Low"
                   , "100000" -- 1 MB
                   , "--High"
                   , "500000" -- 5 MB
                   ])
                10
            , Run
              $ MultiCpu
                (barTheme
                   [ "--template"
                   , "CPU: <total>%"
                   , "--Low"
                   , "40"
                   , "--High"
                   , "80"])
                10
              -- cpu core temperature monitor
            , Run
              $ MultiCoreTemp
                (barTheme
                   [ "--template"
                   , "<fn=1>Temp:</fn> <avg>°"
                   , "--Low"
                   , "1000"
                   , "--High"
                   , "5000"
                   , "--Low"
                   , "60" -- units: °C
                   , "--High"
                   , "80" -- units: °C
                   ])
                50
              -- memory usage monitor
            , Run
              $ Memory
                (barTheme
                   [ "--template"
                   , "Mem: <usedratio>%"
                   , "--Low"
                   , "60" -- units: %
                   , "--High"
                   , "80" -- units: %
                   ])
                10
              -- swap usage monitor
            , Run
              $ Swap
                (barTheme
                   [ "--template"
                   , "<fn=1>swap</fn> <usedratio>%"
                   , "--Low"
                   , "20" -- units: %
                   , "--High"
                   , "80" -- units: %
                   ])
                10]
        }

main :: IO ()
main = do
  args <- getArgs
  palette >>= configFromArgs . config >>= xmobar
