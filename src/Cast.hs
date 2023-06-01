{-# LANGUAGE NamedFieldPuns #-}

module Cast (
  Cast (..),
  Hosts,
  Participants,
  Person,
  Host (..),
  ppCast,
  Storeable (..),
)
where

import Data.Text (Text, intercalate)
import Fmt (fmt, (+|), (|+))

class Storeable x where
  marshall :: x -> Text

newtype Host = Host {unHost :: Text}

instance Storeable Host where
  marshall h = "host " <> unHost h

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
