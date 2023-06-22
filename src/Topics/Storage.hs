module Topics.Storage (
  add,
  remove,
  list,
)
where

import Application (App, AppEnv (topicsFilepath))
import Control.Monad.Trans.Reader (asks)
import qualified Data.Text as T
import qualified Storage as S
import Topics.Topic (Topic (..))

add :: String -> App ()
add strTopic = do
  let topic = T.pack strTopic
  path <- asks topicsFilepath
  S.add path (Topic topic)

remove :: String -> App ()
remove strName = do
  let name = T.pack strName
  path <- asks topicsFilepath
  S.remove path (Topic name)

list :: App [Topic]
list = S.load =<< asks topicsFilepath