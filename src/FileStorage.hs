module FileStorage
  ( load,
    add,
    remove,
    makeFileStorage,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Storage (Storeable (..))
import qualified Storage as S
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, openFile)
import Prelude hiding (appendFile, lines, readFile)

data FileStorage = FileStorage
  { hostsFile :: FilePath,
    participantsFile :: FilePath,
    topicsFile :: FilePath
  }

instance S.Storage FileStorage where
  load = load
  add = add
  remove = remove

makeFileStorage :: IO FileStorage
makeFileStorage = do
  dir <- dataDirectory
  createDirectoryIfMissing True dir

  let fs =
        FileStorage
          { hostsFile = dir </> "people",
            participantsFile = dir </> "people",
            topicsFile = dir </> "topics"
          }
  mapM_ createFileIfMissing [fs.hostsFile, fs.participantsFile, fs.topicsFile]
  pure fs

dataDirectory :: IO FilePath
dataDirectory = do
  e <- lookupEnv "HOME"
  pure $ case e of
    Just path -> path </> ".showtime" </> "data"
    Nothing -> "."

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing path = openFile path AppendMode >>= hClose

load :: (Storeable a) => S.DataType -> FileStorage -> IO [a]
load S.Host fs = loadFile fs.hostsFile
load S.Participant fs = loadFile fs.participantsFile
load S.Topic fs = loadFile fs.topicsFile

loadFile :: (Storeable a) => FilePath -> IO [a]
loadFile path = mapMaybe unmarshall <$> liftIO (readFile path)

add :: (Storeable a) => S.DataType -> a -> FileStorage -> IO ()
add S.Host x fs = addFile fs.hostsFile x
add S.Participant x fs = addFile fs.participantsFile x
add S.Topic x fs = addFile fs.topicsFile x

addFile :: (Storeable a) => FilePath -> a -> IO ()
addFile path x = liftIO $ TIO.appendFile path (marshall x <> "\n")

remove :: (Storeable a) => S.DataType -> a -> FileStorage -> IO ()
remove S.Host x fs = removeFile fs.hostsFile x
remove S.Participant x fs = removeFile fs.participantsFile x
remove S.Topic x fs = removeFile fs.topicsFile x

removeFile :: (Storeable a) => FilePath -> a -> IO ()
removeFile path x = do
  contents <- liftIO $ readFile path
  let contents' = L.delete (marshall x) contents
  liftIO $ TIO.writeFile path (T.unlines contents')

readFile :: FilePath -> IO [Text]
readFile path = T.lines <$> TIO.readFile path
