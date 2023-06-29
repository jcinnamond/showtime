module Cast.Commands (
  runHosts,
  runParticipants,
) where

import Application (App)
import qualified Cast.Storage as Storage
import Control.Monad.IO.Class (liftIO)

runHosts :: [String] -> App ()
runHosts ["add", name] = Storage.addHost name
runHosts ["remove", name] = Storage.removeHost name
runHosts ["list"] = runListHosts
runHosts x = error $ "unexpected command: " <> show x

runListHosts :: App ()
runListHosts = do
  hosts <- Storage.loadHosts
  liftIO $ mapM_ print hosts

runParticipants :: [String] -> App ()
runParticipants ["add", name] = Storage.addParticipant name
runParticipants ["remove", name] = Storage.removeParticipant name
runParticipants ["list"] = runListParticipants
runParticipants x = error $ "unexpected command: " <> show x

runListParticipants :: App ()
runListParticipants = do
  participants <- Storage.loadParticipants
  liftIO $ mapM_ print participants
