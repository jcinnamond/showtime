{-# LANGUAGE GADTs #-}

module Application
  ( AppEnv (..),
    App,
    makeAppEnv,
  )
where

import Control.Monad.Trans.Reader (ReaderT)
import qualified Storage as S

type App s = ReaderT (AppEnv s) IO

data AppEnv s = AppEnv
  { participantCount :: Int,
    storage :: s
  }

makeAppEnv :: (S.Storage s) => s -> AppEnv s
makeAppEnv = AppEnv 2