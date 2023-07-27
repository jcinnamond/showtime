module Cast.Storage
  ( loadCast,
    loadHosts,
    addHost,
    removeHost,
    loadParticipants,
    addParticipant,
    removeParticipant,
  )
where

import Application (App, AppEnv (storage))
import Cast.Cast (Host (..), Participant (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Storage as S

loadHosts :: (S.Storage s) => App s [Host]
loadHosts = do
  s <- asks storage
  liftIO $ S.load S.Host s

loadParticipants :: (S.Storage s) => App s [Participant]
loadParticipants = do
  s <- asks storage
  liftIO $ S.load S.Participant s

loadCast :: (S.Storage s) => App s ([Host], [Participant])
loadCast = do
  hosts <- loadHosts
  participants <- loadParticipants
  pure (hosts, participants)

addHost :: (S.Storage s) => String -> App s ()
addHost = addUnlessExists S.Host Host

removeHost :: (S.Storage s) => String -> App s ()
removeHost name = do
  s <- asks storage
  liftIO $ S.remove S.Host (Host $ T.pack name) s

addParticipant :: (S.Storage s) => String -> App s ()
addParticipant = addUnlessExists S.Participant Participant

removeParticipant :: (S.Storage s) => String -> App s ()
removeParticipant name = do
  s <- asks storage
  liftIO $ S.remove S.Participant (Participant $ T.pack name) s

addUnlessExists :: (S.Storeable a, S.Storage s) => S.DataType -> (Text -> a) -> String -> App s ()
addUnlessExists datatype storable strName = do
  let name = T.pack strName
  exists <- alreadyInCast name
  s <- asks storage
  unless exists $ liftIO $ S.add datatype (storable name) s

alreadyInCast :: (S.Storage s) => Text -> App s Bool
alreadyInCast name = do
  (hosts, participants) <- loadCast
  pure $
    Participant name `elem` participants
      || Host name `elem` hosts