module Storage (
  load,
  add,
  remove,
  loadHosts,
  loadParticipants,
)
where

import AppEnv (AppEnv (filepath))
import Cast (Host (..), Hosts, Participant (..), Participants, Storeable (marshall, name))
import Control.Monad (unless)
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
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

loadHosts :: AppEnv -> IO [Host]
loadHosts env = do
  hosts <- mapMaybe (T.stripPrefix "host ") <$> readFile env
  pure $ Host <$> hosts

loadParticipants :: AppEnv -> IO [Participant]
loadParticipants env = do
  participants <- filter (not . T.isPrefixOf "host ") <$> readFile env
  pure $ Participant <$> participants

add :: (Storeable a) => AppEnv -> a -> IO ()
add env x = do
  contents <- readFile env
  unless (name x `existsIn` contents) $
    TIO.appendFile (T.unpack $ filepath env) (marshall x <> "\n")
 where
  existsIn :: Text -> [Text] -> Bool
  existsIn p ps = (p `elem` ps) || ("host " <> p `elem` ps)

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