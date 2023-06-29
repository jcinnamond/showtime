module Main where

import Application (App, AppEnv (participantCount), loadConfig)
import Cast.Cast (Cast (..), Host (..), Participant (..), ppCast)
import qualified Cast.Commands as CastCommands
import Cast.Storage (loadCast)
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
  go ("hosts" : subCommands) = CastCommands.runHosts subCommands
  go ("participants" : subCommands) = CastCommands.runParticipants subCommands
  go ("topics" : subCommands) = TopicCommands.run subCommands
  go stuff = error $ "unexpected command: " <> show stuff

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
