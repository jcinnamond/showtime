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

import Application (App, AppEnv (peopleFilepath))
import Cast (Host (..), Participant (..))
import Control.Monad (unless)
import Control.Monad.Trans.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Storage as S

loadHosts :: App [Host]
loadHosts = S.load =<< asks peopleFilepath

loadParticipants :: App [Participant]
loadParticipants = S.load =<< asks peopleFilepath

loadCast :: App ([Host], [Participant])
loadCast = do
  hosts <- loadHosts
  participants <- loadParticipants
  pure (hosts, participants)

addHost :: String -> App ()
addHost = addUnlessExists Host

removeHost :: String -> App ()
removeHost name = do
  path <- asks peopleFilepath
  S.remove path (Host $ T.pack name)

addParticipant :: String -> App ()
addParticipant = addUnlessExists Participant

removeParticipant :: String -> App ()
removeParticipant name = do
  path <- asks peopleFilepath
  S.remove path (Participant $ T.pack name)

addUnlessExists :: (S.Storeable s) => (Text -> s) -> String -> App ()
addUnlessExists storable strName = do
  let name = T.pack strName
  exists <- alreadyInCast name
  path <- asks peopleFilepath
  unless exists $ S.add path (storable name)

alreadyInCast :: Text -> App Bool
alreadyInCast name = do
  (hosts, participants) <- loadCast
  pure $
    Participant name `elem` participants
      || Host name `elem` hosts