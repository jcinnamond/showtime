{-# LANGUAGE NamedFieldPuns #-}

module Cast (
  Cast (..),
  Hosts,
  Participants,
  Person,
  Host (..),
  Participant (..),
  ppCast,
  Storeable (..),
)
where

import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Fmt (fmt, (+|), (|+))

class Storeable x where
  marshall :: x -> Text
  name :: x -> Text

newtype Host = Host {getHost :: Text}

instance Storeable Host where
  marshall h = "host " <> getHost h
  name = getHost

instance Show Host where
  show h = T.unpack $ getHost h

newtype Participant = Participant {getParticipant :: Text}

instance Storeable Participant where
  marshall = getParticipant
  name = getParticipant

instance Show Participant where
  show = T.unpack . getParticipant

type Person = Text

type Hosts = [Person]

type Participants = [Person]

data Cast = Cast
  { castHost :: Person
  , castParticipants :: [Person]
  }

ppCast :: Cast -> Text
ppCast Cast{castHost, castParticipants} =
  fmt $ "Host: " +| castHost |+ "\nParticipants: " +| intercalate ", " castParticipants |+ "\n"
