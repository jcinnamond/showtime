module AppEnv
  ( AppEnv (..),
    appEnv,
  )
where

import Data.Text (Text, pack)
import Options.Applicative
import System.Environment (lookupEnv)
import System.FilePath ((</>))

data AppEnv = AppEnv
  { filepath :: Text,
    participantCount :: Int
  }

options :: Text -> Parser AppEnv
options path =
  AppEnv
    <$> strOption
      ( long "filepath"
          <> metavar "TARGET"
          <> help "Path to the file containing the full cast"
          <> value path
      )
    <*> option
      auto
      ( long "participants"
          <> short 'p'
          <> metavar "INT"
          <> help "Number of participants (excluding the host)"
          <> showDefault
          <> value 2
      )

appEnv :: IO AppEnv
appEnv = do
  path <- defaultFilepath
  let opts = info (options path <**> helper) (fullDesc <> progDesc "Pick a random cast")
  execParser opts

defaultFilepath :: IO Text
defaultFilepath = do
  e <- lookupEnv "XDG_CONFIG_HOME"
  pure $ case e of
    Just path -> pack $ path </> "people"
    Nothing -> "people"