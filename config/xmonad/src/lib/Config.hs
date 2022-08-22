module Config (
    Palette (..),
    baseConfig,
    palette,
    (<~>),
    (>~<),
    mkArgs,
    defaultHeight,
    defaultFont,
    fc,
    fn,
    fni,
) where

import System.Environment (lookupEnv)

import Xmobar
import XMonad (Dimension)

defaultHeight :: Dimension
defaultHeight = 32

data Palette = Palette
    { pNormal :: String
    , pLow :: String
    , pHigh :: String
    , pDim :: String
    , pFont :: String
    , pBorder :: String
    , pForeground :: String
    , pBackground :: String
    , pAlpha :: Int
    , pIconRoot :: String
    , pIsLight :: Bool
    , pWm :: Maybe String
    }

fc color thing = "<fc=" ++ color ++ ">" ++ thing ++ "</fc>"
fn n thing = "<fn=" ++ show n ++ ">" ++ thing ++ "</fn>"
fni = fn 6

lightTheme :: IO Bool
lightTheme = fmap (== Just "light") (lookupEnv "JAO_COLOR_SCHEME")

icons k = "/home/jao/.config/xmobar/icons/" ++ k

defaultFont = "xft:Iosevka Extended:pixelsize=32:antialias=true"

lightPalette :: Palette
lightPalette =
    Palette
        { pNormal = "#000000"
        , pLow = "#4d4d4d"
        , pHigh = "#a0522d"
        , pDim = "#999999"
        , pFont = defaultFont
        , pBorder = "#cccccc"
        , pForeground = "#000000"
        , pBackground = "#ffffff"
        , pAlpha = 229
        , pIconRoot = icons "light"
        , pIsLight = True
        , pWm = Nothing
        }

zenburnRed = "#CC9393"
-- zenburnBack = "#2B2B2B"
zenburnBack = "#1f1f1f"
zenburnBackLight = "#383838"
zenburnFg = "#989890" -- "#DCDCCC"
zenburnYl = "#F0DFAF"
zenburnGreen = "#7F9F7F"
doomBack = "#22242b"

darkPalette :: Palette
darkPalette =
    Palette
        { pNormal = zenburnFg
        , pLow = "darkseagreen4" -- zenburnGreen
        , pHigh = zenburnRed
        , pFont = defaultFont
          --                      , pFont = "xft:Noto Sans Mono Medium-9"
          --                      , pFont = "xft:PragmataPro-9"
          --                      , pFont = "xft:Source Code Pro Medium-9"
          --                      , pFont = "xft:IosevkaCC-9"
        , pDim = "#7f7f7f"
          -- , pFont = "xft:NotoMono-9,xft:Inconsolata-11"
        , pBorder = "#000000" -- zenburnBackLight
        , pForeground = zenburnFg
        , pBackground = doomBack -- zenburnBack
        , pAlpha = 255
        , pIconRoot = icons "dark"
        , pIsLight = False
        , pWm = Nothing
        }

palette :: IO Palette
palette = do
    light <- lightTheme
    wm <- lookupEnv "wm"
    let p = if light then lightPalette else darkPalette
    return $ p{pWm = wm}

baseConfig :: Palette -> Config
baseConfig p =
    defaultConfig
        { font = pFont p
        , borderColor = pBorder p
        , fgColor = pForeground p
        , bgColor = pBackground p
        , additionalFonts =
            [ "xft:Symbola-9"
            , "xft:Symbola-10"
            , "xft:Symbola-11"
            , "xft:Symbola-11"
            , "xft:DejaVu Sans Mono-9"
            , --                      , "xft:Noto Sans Mono Medium-9"
              --                      , "xft:IosevkaCC-9"
              "xft:FontAwesome-10"
            ]
        , border = NoBorder
        , alpha = pAlpha p
        , overrideRedirect = True
        , lowerOnStart = True
        , hideOnStart = False
        , allDesktops = True
        , persistent = True
        , sepChar = "|"
        , alignSep = "{}"
        , iconRoot = pIconRoot p
        }

baseTabTheme :: Palette -> Config
baseTabTheme p =
    defaultConfig
        { font = pFont p
        , borderColor = pBorder p
        , fgColor = pForeground p
        , bgColor = pBackground p
        , additionalFonts =
            [ "xft:Symbola-9"
            , "xft:Symbola-10"
            , "xft:Symbola-11"
            , "xft:Symbola-11"
            , "xft:DejaVu Sans Mono-9"
            , --                      , "xft:Noto Sans Mono Medium-9"
              --                      , "xft:IosevkaCC-9"
              "xft:FontAwesome-10"
            ]
        , border = NoBorder
        , alpha = pAlpha p
        , overrideRedirect = True
        , lowerOnStart = True
        , hideOnStart = False
        , allDesktops = True
        , persistent = True
        , sepChar = "|"
        , alignSep = "{}"
        , iconRoot = pIconRoot p
        }
(<~>) :: Palette -> [String] -> [String]
(<~>) p args =
    args ++ ["--low", pLow p, "--normal", pNormal p, "--high", pHigh p]

(>~<) :: Palette -> [String] -> [String]
(>~<) p args =
    args ++ ["--low", pHigh p, "--normal", pNormal p, "--high", pLow p]

mkArgs :: Palette -> [String] -> [String] -> [String]
mkArgs p args extra = concat [p <~> args, ["--"], extra]
