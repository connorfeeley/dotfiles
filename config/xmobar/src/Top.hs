import           Config
import           Monitors
import           Xmobar
import           System.Environment (getArgs)

config p =
  (baseConfig
     p) { position = TopSize L 100 60
        , sepChar = "&" -- delineator between plugin names and straight text
        , alignSep = "}{" -- separator between left-right alignment
        , template = "&enp38s0&      |      &diskio& } &multicoretemp& | &multicpu& | &memory& | GPU: &gputemp&°C { &disku& &swap&"
        , commands =
            [ Run $ XPropertyLog "_XMONAD_LOG_TOP"
              -- free disk space monitor
            , Run
              $ DiskU
                [ ("/home", "home <free>")
                , ("/mnt/ssd", "ssd <free>")
                , ("/", "nix <free>")]
                (barTheme
                   [ "--Low"
                   , "60"
                   , "--High"
                   , "80"
                   , "--minwidth"
                   , "1"
                   , "--ppad"
                   , "3"])
                20
              -- Current org-clock status
            , Run $ ComX "cat" ["/tmp/clock-status"] "" "clock-status" 60
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
            , Run $ DiskIO
                [ ("/home", "home <total> <totalbar>")
                , ("/mnt/ssd", "ssd <total>")
                , ("/", "nix <total> <totalbar>")]
               (barTheme []) 1
              -- cpu activity monitor
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
              -- gpu algo
            , Run
              $ Com
                "bash"
                [ "-c"
                , "{ curl -s localhost:4000/runningminers | jq --raw-output '.[0].Algorithm[0] | tostring | .[0:4]' 2> /dev/null; } || echo 'N/A'"]
                "gpualgo"
                60
              -- gpu hashrate
            , Run
              $ Com
                "bash"
                [ "-c"
                , "{ curl -s localhost:4000/runningminers | jq --raw-output '.[0].\"Speed_Live\"[0] | tostring | .[0:3]' 2> /dev/null; } || echo '0'"]
                "gpurate"
                60
              -- gpu power
            , Run
              $ Com
                "bash"
                [ "-c"
                , "nvidia-smi -q -d POWER | grep 'Power Draw' | cut -d ':' -f2 | cut -d ' ' -f2"
                , "--template"
                , "<gpupower>"]
                "gpupower"
                30
              -- gpu temp
            , Run
              $ Com
                "nvidia-settings"
                ["-t", "-q", "[gpu:0]/GPUCoreTemp"]
                "gputemp"
                30
              -- gpu utilization
            , Run
              $ Com
                "bash"
                [ "-c"
                , "nvidia-settings -t -q '[gpu:0]/GPUUtilization' | cut -d ',' -f1 | cut -d '=' -f2"]
                "gpuutil"
                30
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
