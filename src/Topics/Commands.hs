module Topics.Commands (
  run,
)
where

import AppEnv (AppEnv)
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Topics.Storage as Storage
import Topics.Topic (Topic (getTopic))

run :: AppEnv -> [String] -> IO ()
run env ["add", name] = runAdd env name
run env ["remove", name] = runRemove env name
run env ["list"] = runList env
run _ x = error $ "unexpected command: " <> show x

runAdd :: AppEnv -> String -> IO ()
runAdd = Storage.add

runRemove :: AppEnv -> String -> IO ()
runRemove = Storage.remove

runList :: AppEnv -> IO ()
runList env = do
  topics <- Storage.list env
  forM_ (T.unpack . getTopic <$> topics) putStrLn