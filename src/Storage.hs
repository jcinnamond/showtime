module Storage (
  load,
  add,
  remove,
)
where

import AppEnv (AppEnv (filepath))
import Cast (Hosts, Participants, Storeable (marshall))
import Control.Monad (unless)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (appendFile, lines, readFile)

load :: AppEnv -> IO (Hosts, Participants)
load env = do
  contents <- readFile env
  let (hosts, participants) = L.partition isHost contents
  let hosts' = cleanup <$> hosts
  pure (hosts', hosts' ++ participants)

add :: (Storeable a) => AppEnv -> a -> IO ()
add env x = do
  contents <- readFile env
  let content = marshall x
  unless (content `elem` contents) $
    TIO.appendFile (T.unpack $ filepath env) (content <> "\n")

remove :: (Storeable a) => AppEnv -> a -> IO ()
remove env x = do
  contents <- readFile env
  let contents' = L.delete (marshall x) contents
  TIO.writeFile (T.unpack $ filepath env) (T.unlines contents')

readFile :: AppEnv -> IO [Text]
readFile env = T.lines <$> TIO.readFile (T.unpack $ filepath env)

isHost :: Text -> Bool
isHost = T.isPrefixOf "host "

cleanup :: Text -> Text
cleanup x = fromMaybe x (T.stripPrefix "host " x)