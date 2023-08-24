module Episodes.Commands
  ( run,
  )
where

import Application (App, AppEnv (participantCount))
import Cast.Cast (Cast (..), Host (..), Participant (..))
import qualified Cast.Cast as Cast
import qualified Cast.Storage as CastStorage
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.List (sortOn, unfoldr, (\\))
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Episodes.Episode (Episode (..))
import qualified Episodes.Episode as Episode
import qualified Episodes.Storage as Storage
import qualified Storage as S
import System.Random (RandomGen, getStdGen, uniform)

run :: (S.Storage s) => [String] -> App s ()
run ["new"] = runNew
run ["replace", oldName] = runReplace $ T.pack oldName
run ["show"] = runListEpisodes
run x = error $ "unexpected command: " <> show x

runNew :: (S.Storage s) => App s ()
runNew = do
  cast <- pickCast
  Storage.new $ Episode cast
  liftIO $ TIO.putStrLn $ Cast.pretty cast

pickCast :: (S.Storage s) => App s Cast
pickCast = do
  count <- asks participantCount
  gen <- getStdGen
  (hosts, participants) <- CastStorage.loadCast
  let host = head $ takeRandom gen 1 hosts
  let everyone = (getHost <$> hosts) <> (getParticipant <$> participants)
  let allParticipants = Participant <$> everyone
  let ps = takeRandom gen count allParticipants
  pure $ Cast host ps

takeRandom :: (RandomGen g) => g -> Int -> [a] -> [a]
takeRandom g n xs = take n $ fst <$> sortOn snd (zip xs rs)
  where
    rs :: [Word]
    rs = unfoldr (Just . uniform) g

runListEpisodes :: (S.Storage s) => App s ()
runListEpisodes = do
  episode <- Storage.load
  let p = maybe "no episode planned" Episode.pretty episode
  liftIO $ TIO.putStrLn p

data CastMemberSearchResult
  = ReplaceHost Episode
  | ReplaceParticipant Episode
  | NoEpisode
  | NotFound

runReplace :: (S.Storage s) => Text -> App s ()
runReplace name = do
  episode <- Storage.load
  case findCastMember episode of
    (ReplaceHost e) -> replaceHost name e >> runListEpisodes
    (ReplaceParticipant e) -> replaceParticipant name e >> runListEpisodes
    NoEpisode -> liftIO $ TIO.putStrLn "no episode is planned"
    NotFound -> liftIO $ TIO.putStrLn $ name <> " is not a cast member for the episode"
  where
    findCastMember Nothing = NoEpisode
    findCastMember (Just e)
      | name == (getHost . castHost . getCast) e = ReplaceHost e
      | name `elem` (map getParticipant . castParticipants . getCast) e = ReplaceParticipant e
      | otherwise = NotFound

replaceHost :: (S.Storage s) => Text -> Episode -> App s ()
replaceHost _ episode = do
  gen <- getStdGen
  hosts <- CastStorage.loadHosts
  let potentialHosts = removeCast (episode.getCast) (getHost <$> hosts)
  let newHost = head $ takeRandom gen 1 potentialHosts
  let newEpisode = Episode $ Cast (Host newHost) (castParticipants . getCast $ episode)
  Storage.new newEpisode

replaceParticipant :: (S.Storage s) => Text -> Episode -> App s ()
replaceParticipant name episode = do
  gen <- getStdGen
  hosts <- CastStorage.loadHosts
  participants <- CastStorage.loadParticipants
  let potentialParticipants = removeCast (episode.getCast) ((getHost <$> hosts) <> (getParticipant <$> participants))
  let newParticipant = head $ takeRandom gen 1 potentialParticipants
  let newParticipants = Participant newParticipant : L.delete (Participant name) (castParticipants . getCast $ episode)
  let newEpisode = Episode $ Cast (castHost . getCast $ episode) newParticipants
  Storage.new newEpisode

removeCast :: Cast -> [Text] -> [Text]
removeCast cast names = names \\ castMembers
  where
    participants = getParticipant <$> cast.castParticipants
    host = getHost cast.castHost
    castMembers = host : participants
