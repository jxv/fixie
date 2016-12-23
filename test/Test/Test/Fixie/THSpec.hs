{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Test.Fixie.THSpec (spec) where

import Test.Hspec

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Fail (MonadFail(..))
import Language.Haskell.TH.Syntax

import Test.Fixie
import Test.Fixie.Internal.TH (methodNameToFieldName)

class MultiParam a b where

mkFixture "Fixture" [ts| MonadFail, Quasi |]

spec :: Spec
spec = do
  describe "mkFixture" $
    it "raises an error for multi-parameter typeclasses" $ do
      let fixture = def
            { _qReport = \b s -> when b $ throwError s
            , _qNewName = \s -> return $ Name (OccName s) (NameU 0)
            , _qReify = \_ -> return $(lift =<< reify ''MultiParam)
            }
      let result = outputM fixture (runQ $ mkFixture "Fixture" [ts| MultiParam |])
      result `shouldBe` (Left $
           "mkFixture: cannot derive instance for multi-parameter typeclass\n"
        ++ "      in: Test.Test.Fixie.THSpec.MultiParam\n"
        ++ "      expected: * -> GHC.Types.Constraint\n"
        ++ "      given: * -> * -> GHC.Types.Constraint")

  describe "methodNameToFieldName" $ do
    it "prepends an underscore to ordinary names" $ do
      nameBase (methodNameToFieldName 'id) `shouldBe` "_id"
      nameBase (methodNameToFieldName '_fail) `shouldBe` "__fail"

    it "prepends a tilde to infix operators" $ do
      nameBase (methodNameToFieldName '(>>=)) `shouldBe` "~>>="
      nameBase (methodNameToFieldName '(<|>)) `shouldBe` "~<|>"
