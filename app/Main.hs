module Main where

import AppEnv (AppEnv (participantCount), loadConfig)
import Cast (Cast (..), Host (..), Participant (..), ppCast)
import CastStorage (loadCast)
import qualified CastStorage
import Data.List (sortOn, unfoldr)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Random

main :: IO ()
main = do
  config <- loadConfig
  run config =<< getArgs

run :: AppEnv -> [String] -> IO ()
run env [] = cast env
run env cs = go cs
 where
  go :: [String] -> IO ()
  go ("hosts" : subCommands) = runHosts env subCommands
  go ("participants" : subCommands) = runParticipants env subCommands
  go ["list"] = runList env
  go stuff = error $ "unexpected command: " <> show stuff

runList :: AppEnv -> IO ()
runList env = do
  (hosts, participants) <- CastStorage.loadCast env
  putStrLn "Hosts"
  mapM_ print hosts
  putStrLn "Participants"
  mapM_ print participants

runHosts :: AppEnv -> [String] -> IO ()
runHosts env ["add", name] = runAddHost env name
runHosts env ["remove", name] = runRemoveHost env name
runHosts env ["list"] = runListHosts env
runHosts _ x = error $ "unexpected command: " <> show x

runAddHost :: AppEnv -> String -> IO ()
runAddHost = CastStorage.addHost

runRemoveHost :: AppEnv -> String -> IO ()
runRemoveHost = CastStorage.removeHost

runListHosts :: AppEnv -> IO ()
runListHosts env = CastStorage.loadHosts env >>= mapM_ print

runParticipants :: AppEnv -> [String] -> IO ()
runParticipants env ["add", name] = runAddParticipant env name
runParticipants env ["remove", name] = runRemoveParticipant env name
runParticipants env ["list"] = runListParticipants env
runParticipants _ x = error $ "unexpected command: " <> show x

runAddParticipant :: AppEnv -> String -> IO ()
runAddParticipant = CastStorage.addParticipant

runRemoveParticipant :: AppEnv -> String -> IO ()
runRemoveParticipant = CastStorage.removeParticipant

runListParticipants :: AppEnv -> IO ()
runListParticipants env = CastStorage.loadParticipants env >>= mapM_ print

takeRandom :: (RandomGen g) => g -> Int -> [a] -> [a]
takeRandom g n xs = take n $ fst <$> sortOn snd (zip xs rs)
 where
  rs :: [Word]
  rs = unfoldr (Just . uniform) g

cast :: AppEnv -> IO ()
cast env = do
  gen <- getStdGen
  (hosts, participants) <- loadCast env
  let host = head $ takeRandom gen 1 hosts
  let everyone = (getHost <$> hosts) <> (getParticipant <$> participants)
  let allParticipants = Participant <$> everyone
  let ps = takeRandom gen (participantCount env) allParticipants
  TIO.putStr $ ppCast $ Cast host ps
