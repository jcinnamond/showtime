{-# LANGUAGE InstanceSigs #-}

module HostSpec (spec) where

import Application (makeAppEnv)
import Cast.Cast (Host (..))
import Cast.Storage (loadHosts)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Storage as S
import Test.Hspec (Spec, describe, it, shouldBe)

data MockStorage = MockStorage

instance S.Storage MockStorage where
  load :: (S.Storeable a) => S.DataType -> MockStorage -> IO [a]
  load S.Host _ = pure $ mapMaybe S.unmarshall mockLoadHosts
  load _ _ = error "not implemented"

  add :: (S.Storeable a) => S.DataType -> a -> MockStorage -> IO ()
  add = undefined

  remove :: (S.Storeable a) => S.DataType -> a -> MockStorage -> IO ()
  remove = undefined

  truncate :: S.DataType -> MockStorage -> IO ()
  truncate = undefined

mockLoadHosts :: [Text]
mockLoadHosts = ["host Bob", "host Betty"]

spec :: Spec
spec = do
  describe "hosts" $ do
    it "can be listed" $ do
      let env = makeAppEnv MockStorage
      h <- runReaderT loadHosts env
      h `shouldBe` [Host "Bob", Host "Betty"]
