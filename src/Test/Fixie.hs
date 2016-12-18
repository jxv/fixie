{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Fixie (
  -- * The Fixie monad
    Fixie
  , unFixie
  , logFixie
  , evalFixie
  , execFixie
  , runFixie
  -- * The FixieT monad transformer
  , FixieT
  , unFixieT
  , logFixieT
  , evalFixieT
  , execFixieT
  , runFixieT
  -- * Helper functions
  , module Control.Monad.Writer
  , module Control.Monad.State
  , arg0
  , arg1
  , arg2
  , arg3
  , arg4
  , arg5
  , arg6
  , arg7
  , unimplemented
  , log
  , track
  , captureFunctionCall
  ) where

import Prelude hiding (log)

import qualified Control.Monad.Writer hiding (lift)
import qualified Control.Monad.State hiding (lift)

import Control.Monad.Writer (WriterT, runWriterT)

import Control.Monad.Except
import Control.Monad.RWS
import Data.Functor.Identity
import Data.Either.Combinators (fromRight')
import Data.Text (Text)

type Fixie fixture err log state = FixieT fixture err log state Identity

type Tracker = WriterT [Text]

type FunctionCalls = WriterT [Text]

newtype FixieT fixture err log state m a = FixieT { getRWST :: ExceptT err (RWST (fixture (FixieT fixture err log state m)) [log] state (Tracker (FunctionCalls m))) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (fixture (FixieT fixture err log state m))
    , MonadWriter [log]
    , MonadState state
    )

assumeRight :: Monad m => FixieT fixture err log state m a -> (RWST (fixture (FixieT fixture err log state m)) [log] state (Tracker (FunctionCalls m))) a
assumeRight = fmap fromRight' . runExceptT . getRWST

instance MonadTrans (FixieT fixture error log state) where
  lift = FixieT . lift . lift . lift . lift

instance MonadError e m => MonadError e (FixieT fixture err log state m) where
  throwError = lift . throwError
  catchError m h = FixieT  $ ExceptT $ fmap Right (assumeRight m `catchError` \e -> assumeRight (h e))

unFixieT :: Monad m => FixieT fixture () () () m a -> fixture (FixieT fixture () () () m) -> m a
unFixieT stack env = fmap fst (evalFixieT stack env)

logFixieT :: Monad m => FixieT fixture () log () m a -> fixture (FixieT fixture () log () m) -> m [log]
logFixieT stack env = fmap snd (evalFixieT stack env)

evalFixieT :: Monad m => FixieT fixture () log () m a -> fixture (FixieT fixture () log () m) -> m (a, [log])
evalFixieT stack env = (fmap fst . runWriterT) $ (fmap fst . runWriterT) $ evalRWST (assumeRight stack) env ()

execFixieT :: Monad m => FixieT fixture () log state m a -> fixture (FixieT fixture () log state m) -> state -> m (state, [log])
execFixieT stack env st = (fmap fst . runWriterT) $ (fmap fst . runWriterT) $ execRWST (assumeRight stack) env st

runFixieT :: Monad m => FixieT fixture () log state m a -> fixture (FixieT fixture () log state m) -> state -> m (a, state, [log])
runFixieT stack env st = (fmap fst . runWriterT) $ (fmap fst . runWriterT) $ runRWST (assumeRight stack) env st

unFixie :: Fixie fixture () () () a -> fixture (Fixie fixture () () ()) -> a
unFixie stack env = runIdentity (unFixieT stack env)

logFixie :: Fixie fixture () log () a -> fixture (Fixie fixture () log ()) -> [log]
logFixie stack env = runIdentity (logFixieT stack env)

evalFixie :: Fixie fixture () log () a -> fixture (Fixie fixture () log ()) -> (a, [log])
evalFixie stack env = runIdentity (evalFixieT stack env)

execFixie :: Fixie fixture () log state a -> fixture (Fixie fixture () log state) -> state -> (state, [log])
execFixie stack env st = runIdentity (execFixieT stack env st)

runFixie :: Fixie fixture () log state a -> fixture (Fixie fixture () log state) -> state -> (a, state, [log])
runFixie stack env st = runIdentity (runFixieT stack env st)

arg0 :: (fixture (Fixie fixture () log state) -> Fixie fixture () log state a) -> Fixie fixture () log state a
arg0 rec = join $ asks rec

arg1 :: Monad m => (fixture (FixieT fixture err log state m) -> a -> FixieT fixture err log state m b) -> a -> FixieT fixture err log state m b
arg1 rec a = do
  fn <- asks rec
  fn a

arg2 :: Monad m => (fixture (FixieT fixture err log state m) -> a -> b -> FixieT fixture err log state m c) -> a -> b -> FixieT fixture err log state m c
arg2 rec a b = do
  fn <- asks rec
  fn a b

arg3 :: Monad m => (fixture (FixieT fixture err log state m) -> a -> b -> c -> FixieT fixture err log state m d) -> a -> b -> c -> FixieT fixture err log state m d
arg3 rec a b c = do
  fn <- asks rec
  fn a b c

arg4 :: Monad m => (fixture (FixieT fixture err log state m) -> a -> b -> c -> d -> FixieT fixture err log state m e) -> a -> b -> c -> d -> FixieT fixture err log state m e
arg4 rec a b c d = do
  fn <- asks rec
  fn a b c d

arg5 :: Monad m => (fixture (FixieT fixture err log state m) -> a -> b -> c -> d -> e -> FixieT fixture err log state m f) -> a -> b -> c -> d -> e -> FixieT fixture err log state m f
arg5 rec a b c d e = do
  fn <- asks rec
  fn a b c d e

arg6 :: Monad m => (fixture (FixieT fixture err log state m) -> a -> b -> c -> d -> e -> f -> FixieT fixture err log state m g) -> a -> b -> c -> d -> e -> f -> FixieT fixture err log state m g
arg6 rec a b c d e f = do
  fn <- asks rec
  fn a b c d e f

arg7 :: Monad m => (fixture (FixieT fixture err log state m) -> a -> b -> c -> d -> e -> f -> g -> FixieT fixture err log state m h) -> a -> b -> c -> d -> e -> f -> g -> FixieT fixture err log state m h
arg7 rec a b c d e f g = do
  fn <- asks rec
  fn a b c d e f g

unimplemented :: String -> a
unimplemented name = error ("unimplemented fixture method `" ++ name ++ "`")

log :: MonadWriter [log] m => log -> m ()
log = tell . pure

track :: Monad m => Text -> FixieT fixture err log state m ()
track = FixieT . lift . lift . tell . (:[])

captureFunctionCall :: Monad m => Text -> FixieT fixture err log state m ()
captureFunctionCall = FixieT . lift . lift . lift . tell . (:[])
