module Test.Fixie
  ( module Test.Fixie.Internal
  , mkFixture
  , def
  , ts
  ) where

import Test.Fixie.Internal hiding (Call(..), captureCall, getFixture, getFunction)
import Test.Fixie.Internal.TH (mkFixture)
import Test.Fixie.Internal.TH.TypesQuasi (ts)
import Data.Default.Class (def)
