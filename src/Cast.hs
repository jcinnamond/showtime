{-# LANGUAGE NamedFieldPuns #-}

module Cast
  ( Cast (..),
    Hosts,
    Participants,
    ppCast,
  )
where

import Data.Text (Text, intercalate)
import Fmt (fmt, (+|), (|+))

type Person = Text

type Hosts = [Person]

type Participants = [Person]

data Cast = Cast
  { castHost :: Person,
    castParticipants :: [Person]
  }

ppCast :: Cast -> Text
ppCast Cast {castHost, castParticipants} =
  fmt $ "Host: " +| castHost |+ "\nParticipants: " +| intercalate ", " castParticipants |+ "\n"
