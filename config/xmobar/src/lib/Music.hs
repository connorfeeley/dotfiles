module Music where

-- import Xmobar
-- import Monitors
-- import qualified Bottom
-- import Config (defaultHeight, pIsLight, pHigh, fc, fni)
-- import GMPDP (GMPDP(..))

-- mpris p client width =
--   Mpris2 client
--          ["-t", fni "\xf1bc" ++ " <tracknumber> <title> " ++ fc (pHigh p) "<artist>"
--                 ++ " <album> <length> <composer>"
--          , "-T", show width, "-E", "…", "-M", "100", "-x", ""] 40

-- mprisConfig client p = Bottom.config [Run (mpris p client 165)] "|mpris2|" p

-- mpd = MPD [ "-W", "12", "-b", "░", "-f", "▒", "-t"
--           , " <lapsed> <fc=honeydew3><fn=5><bar></fn></fc>"] 10 -- fn=5


-- mpdt' c0 c1 c2 = "<ppos>/<plength>  "
--                ++ fc c0 "<title> " ++ fc c1 "<artist> " ++ fc c2 "<album>"
--                ++ " <composer> <date>"

-- mpdt light =
--   if light
--   then mpdt' "darkolivegreen" "dodgerblue4" "burlywood4"
--   else mpdt' "darkseagreen4" "darkslategray4" "burlywood4"

-- autoMPD l lgt =
--   AutoMPD [ "-T", l, "-E", "…", "-W", "10", "-t", "<length> " ++ mpdt lgt]

-- mpdConfig p =
--   (Bottom.config [Run mpd, Run (autoMPD "150" (pIsLight p))] "|mpd| |autompd|" p)
--   {
--   textOffsets = [defaultHeight - 7, defaultHeight - 6]
--   }

-- compMPD p = concatMonitor " " mpd (autoMPD "150" (pIsLight p))
-- alt x p = altMonitor (mpris p x 165) (compMPD p)

-- gpmd = Run (GMPDP "gmpdp")

-- config cl p =
--   if cl == "mpd"
--   then mpdConfig p
--   else Bottom.config [Run (alt cl p)] "|mpris2_mpd_autompd|" p
