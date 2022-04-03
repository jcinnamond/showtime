{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (delete, sortOn, unfoldr)
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Fmt
import System.Random

type Person = Text

data Cast = Cast
  { host :: Person,
    participants :: [Person]
  }

ppCast :: Cast -> Text
ppCast Cast {host, participants} =
  fmt $ "Host: " +| host |+ "\nParticipants: " +| intercalate ", " participants |+ "\n"

takeRandom :: RandomGen g => g -> Int -> [a] -> [a]
takeRandom g n xs = take n $ fst <$> sortOn snd (zip xs randoms)
  where
    randoms :: [Word]
    randoms = unfoldr (Just . uniform) g

hosts :: [Person]
hosts = ["Betty", "Freddy"]

people :: [Person]
people = hosts ++ ["Bippy", "Boppy", "Hoppy", "Hetty"]

main :: IO ()
main = do
  gen <- getStdGen
  let host = head $ takeRandom gen 1 hosts
  let ps = takeRandom gen 3 (delete host people)
  TIO.putStr $ ppCast $ Cast host ps