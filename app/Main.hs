module Main where

import AppEnv (AppEnv (participantCount), loadConfig)
import Cast (Cast (..), Host (..), Participant (..), ppCast)
import Data.List (delete, sortOn, unfoldr)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Storage as S
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
  go stuff = error $ "unexpected command: " <> show stuff

runHosts :: AppEnv -> [String] -> IO ()
runHosts env ["add", name] = runAddHost env name
runHosts env ["remove", name] = runRemoveHost env name
runHosts env ["list"] = runListHosts env
runHosts _ x = error $ "unexpected command: " <> show x

runAddHost :: AppEnv -> String -> IO ()
runAddHost env name = S.add env (Host $ T.pack name)

runRemoveHost :: AppEnv -> String -> IO ()
runRemoveHost env name = S.remove env (Host $ T.pack name)

runListHosts :: AppEnv -> IO ()
runListHosts env = S.loadHosts env >>= mapM_ print

runParticipants :: AppEnv -> [String] -> IO ()
runParticipants env ["add", name] = runAddParticipant env name
runParticipants env ["remove", name] = runRemoveParticipant env name
runParticipants env ["list"] = runListParticipants env
runParticipants _ x = error $ "unexpected command: " <> show x

runAddParticipant :: AppEnv -> String -> IO ()
runAddParticipant env name = S.add env (Participant $ T.pack name)

runRemoveParticipant :: AppEnv -> String -> IO ()
runRemoveParticipant env name = S.remove env (Participant $ T.pack name)

runListParticipants :: AppEnv -> IO ()
runListParticipants env = S.loadParticipants env >>= mapM_ print

takeRandom :: (RandomGen g) => g -> Int -> [a] -> [a]
takeRandom g n xs = take n $ fst <$> sortOn snd (zip xs rs)
 where
  rs :: [Word]
  rs = unfoldr (Just . uniform) g

cast :: AppEnv -> IO ()
cast env = do
  gen <- getStdGen
  (hosts, participants) <- S.load env
  let host = head $ takeRandom gen 1 hosts
  let ps = takeRandom gen (participantCount env) (delete host participants)
  TIO.putStr $ ppCast $ Cast host ps
