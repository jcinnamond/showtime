module AppEnv
  ( AppEnv (..),
    loadConfig,
  )
where

import Data.Text (Text, pack)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

data AppEnv = AppEnv
  { filepath :: Text,
    participantCount :: Int
  }

loadConfig :: IO AppEnv
loadConfig = do
  fp <- defaultFilepath
  pure $ AppEnv fp 2

defaultFilepath :: IO Text
defaultFilepath = do
  e <- lookupEnv "XDG_CONFIG_HOME"
  pure $ case e of
    Just path -> pack $ path </> "people"
    Nothing -> "people"