import           Config
import           Monitors
import           Xmobar
import           System.Environment (getArgs)

config p =
  (baseConfig
     p) { position = BottomSize L 100 60
        , sepChar = "&" -- delineator between plugin names and straight text
        , alignSep = "}{" -- separator between left-right alignment
        , template = "&_XMONAD_LOG_BOTTOM& }{ &clock-status& | &date&"
        , commands =
            [ Run $ XPropertyLog "_XMONAD_LOG_BOTTOM"
            -- Current org-clock status
            , Run $ ComX "cat" ["/tmp/clock-status"] "" "clock-status" 60
              -- time and date indicator
            , Run $ Date "<fn=1>%F (%a) %T</fn>" "date" 10]
        }

main :: IO ()
main = do
  args <- getArgs
  palette >>= configFromArgs . config >>= xmobar
