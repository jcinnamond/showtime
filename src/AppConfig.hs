module AppConfig
  ( AppConfig (..),
    loadConfig,
  )
where

import Data.Text (Text, pack)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

data AppConfig = AppConfig
  { filepath :: Text,
    participantCount :: Int
  }

loadConfig :: IO AppConfig
loadConfig = do
  fp <- defaultFilepath
  pure $ AppConfig fp 2

defaultFilepath :: IO Text
defaultFilepath = do
  e <- lookupEnv "XDG_CONFIG_HOME"
  pure $ case e of
    Just path -> pack $ path </> "people"
    Nothing -> "people"