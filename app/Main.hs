{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sortOn, unfoldr)
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Fmt
import System.Random

type Person = Text
data Cast = Cast
  { host :: Person
  , participants :: [Person]
  }

ppCast :: Cast -> Text
ppCast Cast{host, participants} =
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

without :: Eq a => a -> [a] -> [a]
without i [] = []
without i (x : xs) = if i == x then without i xs else x : without i xs

main :: IO ()
main = do
  gen <- getStdGen
  let host = head $ takeRandom gen 1 hosts
  let ps = takeRandom gen 3 (without host people)
  TIO.putStr $ ppCast $ Cast host ps