module Topics.Topic (
  Topic (..),
)
where

import Data.Text (Text)
import qualified Storage as S

newtype Topic = Topic {getTopic :: Text}
  deriving stock (Show, Eq)

instance S.Storeable Topic where
  marshall = getTopic
  unmarshall = Just . Topic
