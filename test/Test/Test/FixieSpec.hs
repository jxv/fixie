{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Test.FixieSpec (spec) where

import Test.Hspec

import Control.Monad.Except (throwError, lift)
import Data.Void (Void)
import Test.Fixie
import Test.Fixie.TH


newtype Id a = Id Int
newtype DBError = DBError () deriving (Eq, Show)

data HTTPRequest = GET String
data HTTPResponse = HTTPResponse { responseStatus :: Int }
data HTTPError

class DBRecord a where
  procureRecord :: a

data User = User deriving (Eq, Show)
instance DBRecord User where
  procureRecord = User

class Monad m => DB m where
  fetchRecord :: DBRecord a => Id a -> m (Either DBError a)
  insertRecord :: DBRecord a => a -> m (Either DBError (Id a))

class Monad m => HTTP m where
  sendRequest :: HTTPRequest -> m (Either HTTPError HTTPResponse)

class Monad m => Throw m where
  throwMessage :: String -> m a

useDBAndHTTP :: (DB m, HTTP m, DBRecord r) => r -> m (Either DBError r)
useDBAndHTTP record = do
  (Right (Id recordId)) <- insertRecord record
  (Right response) <- sendRequest $ GET ("/record/" ++ show recordId)
  fetchRecord $ Id (responseStatus response)

mkFixture "Fixture" [ts| DB, HTTP, Throw |]

fixtureValueM :: Fixture (FixieM Fixture Void)
fixtureValueM = def

fixtureOutputM :: Fixture (FixieM Fixture e)
fixtureOutputM = def

fixtureValueT :: Monad m => Fixture (FixieT Fixture Void m)
fixtureValueT = def

fixtureOutputT :: Monad m => Fixture (FixieT Fixture e m)
fixtureOutputT = def

-- ensure generation of empty fixtures works
mkFixture "EmptyFixture" []

-- ensure fixtures can be generated for partially applied multi-parameter typeclasses
class MultiParam e m | m -> e where
  firstParam :: m e

mkFixture "MultiParamFixture" [ts| MultiParam Bool |]

spec :: Spec
spec = do
  describe "mkFixture" $ do
    it "generates a fixture type that can be used to stub out methods" $ do
      let fixture = def
            { _fetchRecord = \_ -> return $ Right procureRecord
            , _insertRecord = \_ -> return $ Right (Id 42)
            , _sendRequest = \_ -> return $ Right (HTTPResponse 200)
            }
      valueM fixture (useDBAndHTTP User) `shouldBe` Right User

    it "can handle partially applied multi parameter typeclasses" $ do
      let fixture = def { _firstParam = return True }
      valueM fixture firstParam `shouldBe` True

  describe "handle throws" $ do
    it "capture a thrown error message" $ do
      let message = "error message"
      let throw = throwMessage message >> return ()
      let fixture = def
            { _throwMessage = \msg -> do
                lift (msg `shouldBe` message)
                note "test test"
                note "abc"
                throwError msg
            }
      actual <- outputFunctionsNotesT fixture throw
      let expected = (Left "error message", ["throwMessage", "throwError"], ["test test", "abc"])
      actual `shouldBe` expected
