module Storage (
  load,
  add,
  remove,
  Storeable (..),
)
where

import AppEnv (AppEnv (filepath))
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (appendFile, lines, readFile)

class Storeable x where
  marshall :: x -> Text
  unmarshall :: Text -> Maybe x
  name :: x -> Text

load :: (Storeable s) => AppEnv -> IO [s]
load env = mapMaybe unmarshall <$> readFile env

add :: (Storeable a) => AppEnv -> a -> IO ()
add env x = TIO.appendFile (T.unpack $ filepath env) (marshall x <> "\n")

remove :: (Storeable a) => AppEnv -> a -> IO ()
remove env x = do
  contents <- readFile env
  let contents' = L.delete (marshall x) contents
  TIO.writeFile (T.unpack $ filepath env) (T.unlines contents')

readFile :: AppEnv -> IO [Text]
readFile env = T.lines <$> TIO.readFile (T.unpack $ filepath env)
