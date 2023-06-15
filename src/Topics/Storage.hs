module Topics.Storage (
  add,
  remove,
  list,
)
where

import AppEnv (AppEnv (topicsFilepath))
import qualified Data.Text as T
import qualified Storage as S
import Topics.Topic (Topic (..))

add :: AppEnv -> String -> IO ()
add env strTopic = do
  let topic = T.pack strTopic
  S.add (topicsFilepath env) (Topic topic)

remove :: AppEnv -> String -> IO ()
remove env strName = do
  let name = T.pack strName
  S.remove (topicsFilepath env) (Topic name)

list :: AppEnv -> IO [Topic]
list = S.load . topicsFilepath