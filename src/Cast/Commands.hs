module Cast.Commands (
  runHosts,
  runParticipants,
) where

import Application (App)
import qualified Cast.Storage as Storage
import Control.Monad.IO.Class (liftIO)
import qualified Storage as S

runHosts :: (S.Storage s) => [String] -> App s ()
runHosts ["add", name] = Storage.addHost name
runHosts ["remove", name] = Storage.removeHost name
runHosts ["list"] = runListHosts
runHosts x = error $ "unexpected command: " <> show x

runListHosts :: (S.Storage s) => App s ()
runListHosts = do
  hosts <- Storage.loadHosts
  liftIO $ mapM_ print hosts

runParticipants :: (S.Storage s) => [String] -> App s ()
runParticipants ["add", name] = Storage.addParticipant name
runParticipants ["remove", name] = Storage.removeParticipant name
runParticipants ["list"] = runListParticipants
runParticipants x = error $ "unexpected command: " <> show x

runListParticipants :: (S.Storage s) => App s ()
runListParticipants = do
  participants <- Storage.loadParticipants
  liftIO $ mapM_ print participants
