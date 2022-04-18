module Storage
  ( load,
  )
where

import AppEnv (AppEnv (filepath))
import Cast (Hosts, Participants)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, lines, stripPrefix, unpack)
import Data.Text.IO (readFile)
import Prelude hiding (lines, readFile)

load :: AppEnv -> IO (Hosts, Participants)
load config = do
  f <- lines <$> readFile (unpack $ filepath config)
  let (hosts, participants) = partition isHost f
  let hosts' = cleanup <$> hosts
  pure (hosts', hosts' ++ participants)

isHost :: Text -> Bool
isHost = isPrefixOf "host "

cleanup :: Text -> Text
cleanup x = fromMaybe x (stripPrefix "host " x)