module Topics.Storage
  ( add,
    remove,
    list,
  )
where

import Application (App, AppEnv (storage))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Data.Text as T
import qualified Storage as S
import Topics.Topic (Topic (..))

add :: (S.Storage s) => String -> App s ()
add strTopic = do
  let topic = T.pack strTopic
  s <- asks storage
  liftIO $ S.add S.Topic (Topic topic) s

remove :: (S.Storage s) => String -> App s ()
remove strTopic = do
  let topic = T.pack strTopic
  s <- asks storage
  liftIO $ S.remove S.Topic (Topic topic) s

list :: (S.Storage s) => App s [Topic]
list = do
  s <- asks storage
  liftIO $ S.load S.Topic s
