module Main where

import Application (App, AppEnv (participantCount), loadConfig)
import Cast (Cast (..), Host (..), Participant (..), ppCast)
import CastStorage (loadCast)
import qualified CastStorage
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks, runReaderT)
import Data.List (sortOn, unfoldr)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Random
import qualified Topics.Commands as TopicCommands

main :: IO ()
main = do
  args <- getArgs
  runReaderT (run args) =<< loadConfig

run :: [String] -> App ()
run [] = cast
run cs = go cs
 where
  go :: [String] -> App ()
  go ("hosts" : subCommands) = runHosts subCommands
  go ("participants" : subCommands) = runParticipants subCommands
  go ("topics" : subCommands) = TopicCommands.run subCommands
  go ["list"] = runList
  go stuff = error $ "unexpected command: " <> show stuff

runList :: App ()
runList = do
  (hosts, participants) <- CastStorage.loadCast
  liftIO $ do
    putStrLn "Hosts"
    mapM_ print hosts
    putStrLn "Participants"
    mapM_ print participants

runHosts :: [String] -> App ()
runHosts ["add", name] = runAddHost name
runHosts ["remove", name] = runRemoveHost name
runHosts ["list"] = runListHosts
runHosts x = error $ "unexpected command: " <> show x

runAddHost :: String -> App ()
runAddHost = CastStorage.addHost

runRemoveHost :: String -> App ()
runRemoveHost = CastStorage.removeHost

runListHosts :: App ()
runListHosts = do
  hosts <- CastStorage.loadHosts
  liftIO $ mapM_ print hosts

runParticipants :: [String] -> App ()
runParticipants ["add", name] = runAddParticipant name
runParticipants ["remove", name] = runRemoveParticipant name
runParticipants ["list"] = runListParticipants
runParticipants x = error $ "unexpected command: " <> show x

runAddParticipant :: String -> App ()
runAddParticipant = CastStorage.addParticipant

runRemoveParticipant :: String -> App ()
runRemoveParticipant = CastStorage.removeParticipant

runListParticipants :: App ()
runListParticipants = do
  participants <- CastStorage.loadParticipants
  liftIO $ mapM_ print participants

takeRandom :: (RandomGen g) => g -> Int -> [a] -> [a]
takeRandom g n xs = take n $ fst <$> sortOn snd (zip xs rs)
 where
  rs :: [Word]
  rs = unfoldr (Just . uniform) g

cast :: App ()
cast = do
  count <- asks participantCount
  gen <- getStdGen
  (hosts, participants) <- loadCast
  let host = head $ takeRandom gen 1 hosts
  let everyone = (getHost <$> hosts) <> (getParticipant <$> participants)
  let allParticipants = Participant <$> everyone
  let ps = takeRandom gen count allParticipants
  liftIO $ TIO.putStr $ ppCast $ Cast host ps
