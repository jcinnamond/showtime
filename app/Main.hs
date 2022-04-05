module Main where

import AppConfig (AppConfig (participantCount), loadConfig)
import Cast (Cast (..), ppCast)
import Data.List (delete, sortOn, unfoldr)
import qualified Data.Text.IO as TIO
import Storage (load)
import System.Random

takeRandom :: RandomGen g => g -> Int -> [a] -> [a]
takeRandom g n xs = take n $ fst <$> sortOn snd (zip xs rs)
  where
    rs :: [Word]
    rs = unfoldr (Just . uniform) g

main :: IO ()
main = do
  config <- loadConfig
  gen <- getStdGen
  (hosts, participants) <- load config
  let host = head $ takeRandom gen 1 hosts
  let ps = takeRandom gen (participantCount config) (delete host participants)
  TIO.putStr $ ppCast $ Cast host ps