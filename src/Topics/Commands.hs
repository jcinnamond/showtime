module Topics.Commands (
  run,
)
where

import Application (App)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Storage as S
import qualified Topics.Storage as Storage
import Topics.Topic (Topic (getTopic))

run :: (S.Storage s) => [String] -> App s ()
run ["add", name] = Storage.add name
run ["remove", name] = Storage.remove name
run ["list"] = runList
run x = error $ "unexpected command: " <> show x

runList :: (S.Storage s) => App s ()
runList = do
  topics <- Storage.list
  liftIO $ forM_ (T.unpack . getTopic <$> topics) putStrLn