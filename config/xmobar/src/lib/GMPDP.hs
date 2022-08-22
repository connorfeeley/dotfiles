{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json

module GMPDP where

import Data.Aeson


import qualified Data.ByteString.Lazy as B

import GHC.Generics

import Xmobar

import Control.Monad (when, guard)
import Control.Concurrent.STM


import System.INotify (Event(..), EventVariety(..), initINotify, addWatch)

import qualified Data.ByteString.Char8 as BS (ByteString, pack)


-- | Type of each JSON entry in record syntax.
data Song =
  Song { title :: !String
       , artist :: !String
       , album :: !String
       } deriving (Eq,Show,Generic)

data PlayTime =
  PlayTime { current :: !Int
           , total :: !Int
           } deriving (Eq,Show,Generic)

data GMPDPStatus =
  GMPDPStatus { song :: Song
              , time :: PlayTime
              } deriving (Eq,Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON Song
instance FromJSON PlayTime
instance FromJSON GMPDPStatus

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "/home/jao/.config/Google Play Music Desktop Player/json_store/playback.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getGMPDPStatus :: IO (Maybe GMPDPStatus)
getGMPDPStatus = do
    s <- (eitherDecode <$> getJSON) :: IO (Either String GMPDPStatus)
    case s of
      Left _ -> return Nothing
      Right r -> return $ Just r

newtype GMPDP = GMPDP String deriving (Show,Read,Generic)

handleNotification :: TVar (Maybe GMPDPStatus) -> Event -> IO ()
handleNotification v _ =
  getGMPDPStatus >>= \s -> atomically $ writeTVar v s

formatStatus Nothing = ""
formatStatus (Just s) =
  fmtt (current $ time s) ++ "/" ++ fmtt (total $ time s) ++
  " " ++ title (song s) ++ " <fc=sienna4>" ++ album (song s) ++ "</fc>" ++
  " " ++ artist (song s)
  where fmtt ms = let s = ms `div` 1000
                      sr x = if x < 10 then "0" ++ show x else show x
                  in sr (s `div` 60) ++ ":" ++ sr (s `mod` 60)

changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)

instance Exec GMPDP where
  alias (GMPDP a) = a
  start (GMPDP _) cb = do
    i <- initINotify
    s <- getGMPDPStatus
    v <- newTVarIO s
    addWatch i [CloseWrite] (BS.pack jsonFile) (handleNotification v)
    changeLoop (readTVar v) $ \s -> cb (formatStatus s)
