{-# LANGUAGE NoMonomorphismRestriction #-}

module GridSelection where

import qualified Data.Map as M
import           XMonad
import           XMonad.Actions.GridSelect

import           Config

-- | A green monochrome colorizer based on window class
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
  black            -- lowest inactive bg
  (0x70, 0xFF, 0x70) -- highest inactive bg
  black            -- active bg
  white            -- inactive fg
  white            -- active fg
  where
    black = minBound

    white = maxBound

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap = M.fromList
      [ ((0, xK_Escape), cancel)
      , ((0, xK_Return), select)
      , ((0, xK_slash), substringSearch myNavigation)
      , ((0, xK_h), move (-1, 0) >> myNavigation)
      , ((0, xK_l), move (1, 0) >> myNavigation)
      , ((0, xK_j), move (0, 1) >> myNavigation)
      , ((0, xK_k), move (0, -1) >> myNavigation)
      , ((0, xK_space), setPos (0, 0) >> myNavigation)]

    -- The navigation handler ignores unknown key symbols
    navDefaultHandler = const myNavigation

myGSConfig :: p -> GSConfig Window
myGSConfig colorizer =
  (buildDefaultGSConfig myColorizer) { gs_cellheight = 30
                                     , gs_cellwidth = 100
                                     , gs_navigate = myNavigation
                                     }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where
    conf = def { gs_cellheight = 2 * toInteger defaultHeight
               , gs_cellwidth = 200
               , gs_cellpadding = 6
               , gs_originFractX = 0.5 -- X position: 50%
               , gs_originFractY = 0.5 -- Y position: 50%
               , gs_font = defaultFont
               , gs_navigate = myNavigation
               }

-- Entry for GridSelection in myConfig::{additionalKeys}
-- TODO: add 'pwrbar' controls
gsLaunch mod4Mask =
  ( (mod4Mask, xK_s)
  , spawnSelected'
      [ ("Chromium", "chromium")
      , ("LibreWolf", "librewolf")
      , ("Remmina", "remmina")
      , ("Kitty", "kitty")
      , ("Emacs", "e")])
