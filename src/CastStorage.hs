module CastStorage (
  loadCast,
  loadHosts,
  addHost,
  removeHost,
  loadParticipants,
  addParticipant,
  removeParticipant,
)
where

import AppEnv (AppEnv)
import Cast (Host (..), Participant (..))
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Storage as S

loadHosts :: AppEnv -> IO [Host]
loadHosts = S.load

loadParticipants :: AppEnv -> IO [Participant]
loadParticipants = S.load

loadCast :: AppEnv -> IO ([Host], [Participant])
loadCast env = do
  hosts <- loadHosts env
  participants <- loadParticipants env
  pure (hosts, participants)

addHost :: AppEnv -> String -> IO ()
addHost = addUnlessExists Host

removeHost :: AppEnv -> String -> IO ()
removeHost env name = S.remove env (Host $ T.pack name)

addParticipant :: AppEnv -> String -> IO ()
addParticipant = addUnlessExists Participant

removeParticipant :: AppEnv -> String -> IO ()
removeParticipant env name = S.remove env (Participant $ T.pack name)

addUnlessExists :: (S.Storeable s) => (Text -> s) -> AppEnv -> String -> IO ()
addUnlessExists storable env strName = do
  let name = T.pack strName
  exists <- alreadyInCast env name
  unless exists $ S.add env (storable name)

alreadyInCast :: AppEnv -> Text -> IO Bool
alreadyInCast env name = do
  (hosts, participants) <- loadCast env
  pure $
    Participant name `elem` participants
      || Host name `elem` hosts