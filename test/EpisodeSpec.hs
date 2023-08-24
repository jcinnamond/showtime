{-# LANGUAGE InstanceSigs #-}

module EpisodeSpec (spec) where

import Application (makeAppEnv)
import Cast.Cast (Cast (..), Host (Host), Participant (Participant))
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import Episodes.Episode (Episode (..))
import Episodes.Storage as ES
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import qualified Storage as S
import Test.Hspec

newtype MockStorage = MockStorage {commands :: TVar [Text]}

instance S.Storage MockStorage where
  load :: (S.Storeable a) => S.DataType -> MockStorage -> IO [a]
  load _ ms = storeCommand ms "load" >> pure []

  add :: (S.Storeable a) => S.DataType -> a -> MockStorage -> IO ()
  add _ _ ms = storeCommand ms "add"

  remove :: (S.Storeable a) => S.DataType -> a -> MockStorage -> IO ()
  remove _ _ ms = storeCommand ms "remove"

  truncate :: S.DataType -> MockStorage -> IO ()
  truncate _ ms = storeCommand ms "truncate"

storeCommand :: MockStorage -> Text -> IO ()
storeCommand ms c = do
  atomically $ do
    cs <- readTVar (commands ms)
    writeTVar (commands ms) $ c : cs

spec :: Spec
spec = do
  describe "add called multiple times" $ do
    it "truncates the file before adding the episode" $ do
      commands <- newTVarIO []
      let s = MockStorage commands
      let env = makeAppEnv s
      let e = Episode $ Cast (Host "Bippy") [Participant "Boppy", Participant "Buppy"]

      runReaderT (ES.new e) env
      actualCommands <- readTVarIO commands
      actualCommands `shouldBe` ["add", "truncate"]
