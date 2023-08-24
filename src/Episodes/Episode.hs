{-# LANGUAGE InstanceSigs #-}

module Episodes.Episode (
  Episode (..),
  pretty,
)
where

import Cast.Cast (Cast (..))
import qualified Cast.Cast as Cast
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Storage (Storeable (..))

newtype Episode = Episode {getCast :: Cast}
  deriving stock (Eq, Show)

instance Storeable Episode where
  marshall = marshallEpisode
  unmarshall = unmarshallEpisode

marshallEpisode :: Episode -> Text
marshallEpisode (Episode cast) =
  L.foldl (<>) "" $
    L.intersperse "\0" $
      [marshall cast.castHost] <> (marshall <$> cast.castParticipants)

unmarshallEpisode :: Text -> Maybe Episode
unmarshallEpisode t = do
  let ts = T.splitOn "\0" t
  let host = firstJust unmarshall ts
  let participants = mapMaybe unmarshall ts
  case host of
    Just h -> Just $ Episode $ Cast h participants
    Nothing -> Nothing

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f xs = case mapMaybe f xs of
  (x : _) -> Just x
  _ -> Nothing

pretty :: Episode -> Text
pretty = Cast.pretty . getCast