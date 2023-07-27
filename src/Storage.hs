module Storage
  ( Storage (..),
    Storeable (..),
    DataType (..),
  )
where

import Data.Text (Text)

data DataType = Host | Participant | Topic
  deriving (Show, Eq, Ord)

class Storage x where
  load :: (Storeable a) => DataType -> x -> IO [a]
  add :: (Storeable a) => DataType -> a -> x -> IO ()
  remove :: (Storeable a) => DataType -> a -> x -> IO ()

class Storeable x where
  marshall :: x -> Text
  unmarshall :: Text -> Maybe x
