module AppEnv (
  AppEnv (..),
  loadConfig,
)
where

import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (IOMode (AppendMode), hClose, openFile)

data AppEnv = AppEnv
  { peopleFilepath :: FilePath
  , topicsFilepath :: FilePath
  , participantCount :: Int
  }

loadConfig :: IO AppEnv
loadConfig = do
  pp <- getFilepath "people"
  tp <- getFilepath "topics"
  pure $ AppEnv pp tp 2

getFilepath :: FilePath -> IO FilePath
getFilepath f = do
  dataDir <- dataDirectory
  createDirectoryIfMissing True dataDir
  let path = dataDir </> f
  createFileIfMissing path
  pure $ dataDir </> f

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing path = openFile path AppendMode >>= hClose

dataDirectory :: IO FilePath
dataDirectory = do
  e <- lookupEnv "HOME"
  pure $ case e of
    Just path -> path </> ".showtime" </> "data"
    Nothing -> "."