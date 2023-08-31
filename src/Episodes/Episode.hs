module Episodes.Episode (
  Episode (..),
  pretty,
)
where

import Cast.Cast (Cast (..), Host (..), Participant (..))
import qualified Cast.Cast as Cast
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Storage (Storeable (..))
import Topics.Topic (Topic (..))
import qualified Topics.Topic as Topic

data Episode = Episode
  { getCast :: Cast
  , suggestedTopics :: [Topic]
  , topic :: Maybe Topic
  }
  deriving stock (Eq, Show)

instance Storeable Episode where
  marshall = marshallEpisode
  unmarshall = unmarshallEpisode

marshallEpisode :: Episode -> Text
marshallEpisode (Episode cast topics topic) =
  L.foldl (<>) "" $
    L.intersperse "\0" $
      [marshallHost] <> marshallParticipant <> marshallSuggestedTopics <> [marshallTopic]
 where
  marshallHost = "host " <> cast.castHost.getHost
  marshallParticipant = map ("participant " <>) (Cast.getParticipant <$> cast.castParticipants)
  marshallSuggestedTopics = map ("suggested topic " <>) (Topic.getTopic <$> topics)
  marshallTopic = maybe "" (("topic " <>) . getTopic) topic

unmarshallEpisode :: Text -> Maybe Episode
unmarshallEpisode t = do
  let ts = T.splitOn "\0" t
  let host = firstJust unmarshallHost ts
  let participants = mapMaybe unmarshallParticipant ts
  let topics = mapMaybe unmarshallSuggestedTopic ts
  let topic = firstJust unmarshallTopic ts
  case host of
    Just h -> Just $ Episode (Cast h participants) topics topic
    Nothing -> Nothing
 where
  unmarshallHost x = Host <$> T.stripPrefix "host " x
  unmarshallParticipant x = Participant <$> T.stripPrefix "participant " x
  unmarshallSuggestedTopic x = Topic <$> T.stripPrefix "suggested topic " x
  unmarshallTopic x = Topic <$> T.stripPrefix "topic " x

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f xs = case mapMaybe f xs of
  (x : _) -> Just x
  _ -> Nothing

pretty :: Episode -> Text
pretty e@(Episode cast _ _) =
  Cast.pretty cast <> prettyTopics e

prettyTopics :: Episode -> Text
prettyTopics (Episode _ _ (Just t)) = "\nTopic: " <> t.getTopic
prettyTopics (Episode _ topics Nothing) = prettySuggestedTopics topics

prettySuggestedTopics :: [Topic] -> Text
prettySuggestedTopics [] = ""
prettySuggestedTopics topics =
  "\nSuggested topics:\n"
    <> T.unlines (Topic.getTopic <$> topics)