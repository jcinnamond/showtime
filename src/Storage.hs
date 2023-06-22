module Storage (
  load,
  add,
  remove,
  Storeable (..),
)
where

import Application (App)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (appendFile, lines, readFile)

class Storeable x where
  marshall :: x -> Text
  unmarshall :: Text -> Maybe x

load :: (Storeable s) => FilePath -> App [s]
load path = mapMaybe unmarshall <$> liftIO (readFile path)

add :: (Storeable a) => FilePath -> a -> App ()
add path x = liftIO $ TIO.appendFile path (marshall x <> "\n")

remove :: (Storeable a) => FilePath -> a -> App ()
remove path x = do
  contents <- liftIO $ readFile path
  let contents' = L.delete (marshall x) contents
  liftIO $ TIO.writeFile path (T.unlines contents')

readFile :: FilePath -> IO [Text]
readFile path = T.lines <$> TIO.readFile path
