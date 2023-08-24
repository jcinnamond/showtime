module Episodes.Storage
  ( new,
    load,
  )
where

import Application (App, AppEnv (storage))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Episodes.Episode (Episode)
import qualified Storage as S

new :: (S.Storage s) => Episode -> App s ()
new episode = do
  s <- asks storage
  liftIO $ do
    S.truncate S.Episode s
    S.add S.Episode episode s
  pure ()

load :: (S.Storage s) => App s (Maybe Episode)
load = do
  s <- asks storage
  episodes <- liftIO $ S.load S.Episode s
  case episodes of
    [] -> pure Nothing
    (x : _) -> pure $ Just x
