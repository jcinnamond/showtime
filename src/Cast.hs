{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cast (
  Cast (..),
  Host (..),
  Participant (..),
  ppCast,
)
where

import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Fmt (fmt, (+|), (|+))
import Storage (Storeable (..))

newtype Host = Host {getHost :: Text}
  deriving stock (Eq)

hostPrefix :: Text
hostPrefix = "host "

instance Storeable Host where
  marshall h = hostPrefix <> getHost h
  unmarshall t = Host <$> T.stripPrefix hostPrefix t
  name = getHost

instance Show Host where
  show h = T.unpack $ getHost h

newtype Participant = Participant {getParticipant :: Text}
  deriving stock (Eq)

instance Storeable Participant where
  marshall = getParticipant
  unmarshall t = Participant <$> withoutHostPrefix t
  name = getParticipant

withoutHostPrefix :: Text -> Maybe Text
withoutHostPrefix x = if T.isPrefixOf hostPrefix x then Nothing else Just x

instance Show Participant where
  show = T.unpack . getParticipant

data Cast = Cast
  { castHost :: Host
  , castParticipants :: [Participant]
  }

ppCast :: Cast -> Text
ppCast Cast{castHost, castParticipants} =
  fmt $ "Host: " +| getHost castHost |+ "\nParticipants: " +| intercalate ", " (map getParticipant castParticipants) |+ "\n"
