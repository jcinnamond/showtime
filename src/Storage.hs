module Storage
  ( load,
  )
where

import Cast (Hosts, Participants)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, lines, stripPrefix)
import Data.Text.IO (readFile)
import Prelude hiding (lines, readFile)

load :: IO (Hosts, Participants)
load = do
  f <- lines <$> readFile "people"
  let (hosts, participants) = partition isHost f
  let hosts' = cleanup <$> hosts
  pure (hosts', hosts' ++ participants)

isHost :: Text -> Bool
isHost = isPrefixOf "host "

cleanup :: Text -> Text
cleanup x = fromMaybe x (stripPrefix "host " x)