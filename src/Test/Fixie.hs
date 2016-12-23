{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Fixie
  ( Note(..)
  , Function(..)
  , Call(..)
  , FixieT
  , FixieM
  , toSet
  , note
  , getFixture
  , getFunction
  , captureCall
  , unimplemented
  , outputNotesFunctionsT
  , outputNotesFunctionsetT
  , outputFunctionsNotesT
  , outputFunctionsetNotesT
  , valueNotesFunctionsT
  , valueNotesFunctionsetT
  , valueFunctionsNotesT
  , valueFunctionsetNotesT
  , notesOutputFunctionsT
  , notesOutputFunctionsetT
  , notesValueFunctionsT
  , notesValueFunctionsetT
  , notesFunctionsOutputT
  , notesFunctionsetOutputT
  , notesFunctionsValueT
  , notesFunctionsetValueT
  , functionsOutputNotesT
  , functionsValueNotesT
  , functionsNotesOutputT
  , functionsNotesValueT
  , functionsetOutputNotesT
  , functionsetValueNotesT
  , functionsetNotesOutputT
  , functionsetNotesValueT
  , outputNotesT
  , outputFunctionsT
  , outputFunctionsetT
  , valueNotesT
  , valueFunctionsT
  , valueFunctionsetT
  , notesOutputT
  , notesValueT
  , notesFunctionsT
  , notesFunctionsetT
  , functionsOutputT
  , functionsValueT
  , functionsNotesT
  , functionsetOutputT
  , functionsetValueT
  , functionsetNotesT
  , outputT
  , valueT
  , notesT
  , functionsT
  , functionsetT
  , outputNotesFunctionsM
  , outputNotesFunctionsetM
  , outputFunctionsNotesM
  , outputFunctionsetNotesM
  , valueNotesFunctionsM
  , valueNotesFunctionsetM
  , valueFunctionsNotesM
  , valueFunctionsetNotesM
  , notesOutputFunctionsM
  , notesOutputFunctionsetM
  , notesValueFunctionsM
  , notesValueFunctionsetM
  , notesFunctionsOutputM
  , notesFunctionsetOutputM
  , notesFunctionsValueM
  , notesFunctionsetValueM
  , functionsOutputNotesM
  , functionsValueNotesM
  , functionsNotesOutputM
  , functionsNotesValueM
  , functionsetOutputNotesM
  , functionsetValueNotesM
  , functionsetNotesOutputM
  , functionsetNotesValueM
  , outputNotesM
  , outputFunctionsM
  , outputFunctionsetM
  , valueNotesM
  , valueFunctionsM
  , valueFunctionsetM
  , notesOutputM
  , notesValueM
  , notesFunctionsM
  , notesFunctionsetM
  , functionsOutputM
  , functionsValueM
  , functionsNotesM
  , functionsetOutputM
  , functionsetValueM
  , functionsetNotesM
  , outputM
  , valueM
  , notesM
  , functionsM
  , functionsetM
  ) where

import Prelude hiding (log)

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Writer (WriterT, runWriterT)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Identity
import Data.String (IsString)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Either.Combinators (fromRight')
import Data.Text (Text)
import Data.Void (Void)

newtype Note = Note Text
  deriving (Show, Eq, IsString)

newtype Function = Function Text
  deriving (Show, Eq, Ord, IsString)

newtype Call = Call { _function :: Function }
  deriving (Show, Eq, Ord)

newtype FixieT f e m a = FixieT (ExceptT e (ReaderT (f (FixieT f e m)) (WriterT [Note] (WriterT [Call] m))) a)
  deriving (Functor, Applicative, Monad, MonadError e)

instance MonadTrans (FixieT f e) where
  lift = FixieT . lift . lift . lift . lift

type FixieM f e = FixieT f e Identity

--

unimplemented :: String -> a
unimplemented name = error ("unimplemented fixture method `" ++ name ++ "`")

toSet :: Ord a => [a] -> Set a
toSet = Set.fromList

--

getFixture :: Monad m => FixieT f e m (f (FixieT f e m))
getFixture = FixieT $ lift ask

getFunction :: Monad m => (f (FixieT f e m) -> a) -> FixieT f e m a
getFunction f = FixieT $ lift (asks f)

note :: Monad m => Note -> FixieT f e m ()
note = FixieT . lift . lift . tell . (:[])

captureCall :: Monad m => Call -> FixieT f e m ()
captureCall = FixieT . lift . lift . lift . tell . (:[])

--

noVoid :: Either Void a -> a
noVoid = fromRight'

pluck :: (a, b, c) -> (a, b)
pluck (a, b, _) = (a, b)

swap_0_2_1 :: (a, b, c) -> (a, c, b)
swap_0_2_1 (a, b, c) = (a, c, b)

swap_1_0_2 :: (a, b, c) -> (b, a, c)
swap_1_0_2 (a, b, c) = (b, a, c)

swap_1_2_0 :: (a, b, c) -> (b, c, a)
swap_1_2_0 (a, b, c) = (b, c, a)

swap_2_0_1 :: (a, b, c) -> (c, a, b)
swap_2_0_1 (a, b, c) = (c, a, b)

swap_2_1_0 :: (a, b, c) -> (c, b, a)
swap_2_1_0 (a, b, c) = (c, b, a)

map_0 :: (a -> d) -> (a, b, c) -> (d, b, c)
map_0 f (a, b, c) = (f a, b, c)

map_1 :: (b -> d) -> (a, b, c) -> (a, d, c)
map_1 f (a, b, c) = (a, f b, c)

map_2 :: (c -> d) -> (a, b, c) -> (a, b, d)
map_2 f (a, b, c) = (a, b, f c)

--

outputNotesCalls :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, [Note], [Call])
outputNotesCalls f (FixieT m) = fmap flattenTuple $ runWriterT $ runWriterT $ runReaderT (runExceptT m) f
  where
    flattenTuple :: ((a, b), c) -> (a, b, c)
    flattenTuple ((a, b), c) = (a, b, c)

--

valueNotesCalls :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, [Note], [Call])
valueNotesCalls f x = strip <$> outputNotesCalls f x
  where
    strip :: (Either Void a, b, c) -> (a, b, c) 
    strip (a, b, c) = (noVoid a, b, c)

--

outputNotesFunctions :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, [Note], [Function])
outputNotesFunctions f x = fn <$> outputNotesCalls f x
  where
    fn :: (Either e a, [Note], [Call]) -> (Either e a, [Note], [Function])
    fn (a, b, c) = (a, b, map _function c)

valueNotesFunctions :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, [Note], [Function])
valueNotesFunctions f x = fn <$> valueNotesCalls f x
  where
    fn :: (a, b, [Call]) -> (a, b, [Function]) 
    fn (a, b, c) = (a, b, map _function c)

----

outputNotesFunctionsT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, [Note], [Function])
outputNotesFunctionsT = outputNotesFunctions

outputNotesFunctionsetT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, [Note], Set Function)
outputNotesFunctionsetT f x = map_2 toSet <$>  outputNotesFunctions f x

outputFunctionsNotesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, [Function], [Note])
outputFunctionsNotesT f x = swap_0_2_1 <$> outputNotesFunctions f x

outputFunctionsetNotesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, Set Function, [Note])
outputFunctionsetNotesT f x = map_1 toSet . swap_0_2_1 <$> outputNotesFunctions f x

valueNotesFunctionsT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, [Note], [Function])
valueNotesFunctionsT = valueNotesFunctions

valueNotesFunctionsetT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, [Note], Set Function)
valueNotesFunctionsetT f x = map_2 toSet <$> valueNotesFunctions f x

valueFunctionsNotesT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, [Function], [Note])
valueFunctionsNotesT f x = swap_0_2_1 <$> valueNotesFunctions f x

valueFunctionsetNotesT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, Set Function, [Note])
valueFunctionsetNotesT f x = map_1 toSet . swap_0_2_1 <$> valueNotesFunctions f x

--

notesOutputFunctionsT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Note], Either e a, [Function])
notesOutputFunctionsT f x = swap_1_0_2 <$> outputNotesFunctions f x

notesOutputFunctionsetT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Note], Either e a, Set Function)
notesOutputFunctionsetT f x = map_2 toSet . swap_1_0_2 <$> outputNotesFunctions f x

notesValueFunctionsT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Note], a, [Function])
notesValueFunctionsT f x = swap_1_0_2 <$> valueNotesFunctions f x

notesValueFunctionsetT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Note], a, Set Function)
notesValueFunctionsetT f x = map_2 toSet . swap_1_0_2 <$> valueNotesFunctions f x

notesFunctionsOutputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Note], [Function], Either e a)
notesFunctionsOutputT f x = swap_1_2_0 <$> outputNotesFunctions f x

notesFunctionsetOutputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Note], Set Function,  Either e a)
notesFunctionsetOutputT f x = map_1 toSet . swap_1_2_0 <$> outputNotesFunctions f x

notesFunctionsValueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Note], [Function], a)
notesFunctionsValueT f x = swap_1_2_0 <$> valueNotesFunctions f x

notesFunctionsetValueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Note], Set Function, a)
notesFunctionsetValueT f x = map_1 toSet . swap_1_2_0 <$> valueNotesFunctions f x

functionsOutputNotesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Function], Either e a, [Note])
functionsOutputNotesT f x = swap_2_0_1 <$> outputNotesFunctions f x

functionsValueNotesT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Function], a, [Note])
functionsValueNotesT f x = swap_2_0_1 <$> valueNotesFunctions f x

functionsNotesOutputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Function], [Note], Either e a)
functionsNotesOutputT f x = swap_2_1_0 <$> outputNotesFunctions f x

functionsNotesValueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Function], [Note], a)
functionsNotesValueT f x = swap_2_1_0 <$> valueNotesFunctions f x

functionsetOutputNotesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Set Function, Either e a, [Note])
functionsetOutputNotesT f x = map_0 toSet . swap_2_0_1 <$> outputNotesFunctions f x

functionsetValueNotesT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (Set Function, a, [Note])
functionsetValueNotesT f x = map_0 toSet . swap_2_0_1 <$> valueNotesFunctions f x

functionsetNotesOutputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Set Function, [Note], Either e a)
functionsetNotesOutputT f x = map_0 toSet . swap_2_1_0 <$> outputNotesFunctions f x

functionsetNotesValueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (Set Function, [Note], a)
functionsetNotesValueT f x = map_0 toSet . swap_2_1_0 <$> valueNotesFunctions f x

--

outputNotesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, [Note])
outputNotesT f x = pluck <$> outputNotesFunctions f x

outputFunctionsT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, [Function])
outputFunctionsT f x = pluck . swap_0_2_1 <$> outputNotesFunctions f x

outputFunctionsetT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a, Set Function)
outputFunctionsetT f x = pluck . map_1 toSet . swap_0_2_1 <$> outputNotesFunctions f x

valueNotesT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, [Note])
valueNotesT f x = pluck <$> valueNotesFunctions f x

valueFunctionsT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, [Function])
valueFunctionsT f x = pluck . swap_0_2_1 <$> valueNotesFunctions f x

valueFunctionsetT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (a, Set Function)
valueFunctionsetT f x = pluck . map_1 toSet . swap_0_2_1 <$> valueNotesFunctions f x

notesOutputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Note], Either e a)
notesOutputT f x = pluck . swap_1_0_2 <$> outputNotesFunctions f x

notesValueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Note], a)
notesValueT f x = pluck . swap_1_0_2 <$> valueNotesFunctions f x

notesFunctionsT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Note], [Function])
notesFunctionsT f x = pluck . swap_1_2_0 <$> outputNotesFunctions f x

notesFunctionsetT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Note], Set Function)
notesFunctionsetT f x = pluck . map_1 toSet . swap_1_2_0 <$> outputNotesFunctions f x

functionsOutputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Function], Either e a)
functionsOutputT f x = pluck . swap_2_0_1 <$> outputNotesFunctions f x

functionsValueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m ([Function], a)
functionsValueT f x = pluck . swap_2_0_1 <$> valueNotesFunctions f x

functionsNotesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m ([Function], [Note])
functionsNotesT f x = pluck . swap_2_1_0 <$> outputNotesFunctions f x

functionsetOutputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Set Function, Either e a)
functionsetOutputT f x = pluck . map_0 toSet . swap_2_0_1 <$> outputNotesFunctions f x

functionsetValueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m (Set Function, a)
functionsetValueT f x = pluck . map_0 toSet . swap_2_0_1 <$> valueNotesFunctions f x

functionsetNotesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Set Function, [Note])
functionsetNotesT f x = pluck . map_0 toSet . swap_2_1_0 <$> outputNotesFunctions f x

outputT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Either e a)
outputT f x = (\(a,_,_) -> a) <$> outputNotesFunctions f x

valueT :: Monad m => f (FixieT f Void m) -> FixieT f Void m a -> m a
valueT f x = (\(a,_,_) -> a) <$> valueNotesFunctions f x

notesT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m [Note]
notesT f x = (\(_,b,_) -> b) <$> outputNotesFunctions f x

functionsT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m [Function]
functionsT f x = (\(_,_,c) -> c) <$> outputNotesFunctions f x

functionsetT :: Monad m => f (FixieT f e m) -> FixieT f e m a -> m (Set Function)
functionsetT f x = (\(_,_,c) -> toSet c) <$> outputNotesFunctions f x

--------

fixieM :: (a -> b -> Identity c) -> a -> b -> c
fixieM y f x = runIdentity (y f x)

outputNotesFunctionsM :: f (FixieM f e) -> FixieM f e a -> (Either e a, [Note], [Function])
outputNotesFunctionsM = fixieM outputNotesFunctionsT 

outputNotesFunctionsetM :: f (FixieM f e) -> FixieM f e a -> (Either e a, [Note], Set Function)
outputNotesFunctionsetM = fixieM outputNotesFunctionsetT

outputFunctionsNotesM :: f (FixieM f e) -> FixieM f e a -> (Either e a, [Function], [Note])
outputFunctionsNotesM = fixieM outputFunctionsNotesT

outputFunctionsetNotesM :: f (FixieM f e) -> FixieM f e a -> (Either e a, Set Function, [Note])
outputFunctionsetNotesM = fixieM outputFunctionsetNotesT

valueNotesFunctionsM :: f (FixieM f Void) -> FixieM f Void a -> (a, [Note], [Function])
valueNotesFunctionsM = fixieM valueNotesFunctionsT

valueNotesFunctionsetM :: f (FixieM f Void) -> FixieM f Void a -> (a, [Note], Set Function)
valueNotesFunctionsetM = fixieM valueNotesFunctionsetT

valueFunctionsNotesM :: f (FixieM f Void) -> FixieM f Void a -> (a, [Function], [Note])
valueFunctionsNotesM = fixieM valueFunctionsNotesT

valueFunctionsetNotesM :: f (FixieM f Void) -> FixieM f Void a -> (a, Set Function, [Note])
valueFunctionsetNotesM = fixieM valueFunctionsetNotesT

notesOutputFunctionsM :: f (FixieM f e) -> FixieM f e a -> ([Note], Either e a, [Function])
notesOutputFunctionsM = fixieM notesOutputFunctionsT

notesOutputFunctionsetM :: f (FixieM f e) -> FixieM f e a -> ([Note], Either e a, Set Function)
notesOutputFunctionsetM = fixieM notesOutputFunctionsetT

notesValueFunctionsM :: f (FixieM f Void) -> FixieM f Void a -> ([Note], a, [Function])
notesValueFunctionsM = fixieM notesValueFunctionsT

notesValueFunctionsetM :: f (FixieM f Void) -> FixieM f Void a -> ([Note], a, Set Function)
notesValueFunctionsetM = fixieM notesValueFunctionsetT

notesFunctionsOutputM :: f (FixieM f e) -> FixieM f e a -> ([Note], [Function], Either e a)
notesFunctionsOutputM = fixieM notesFunctionsOutputT

notesFunctionsetOutputM :: f (FixieM f e) -> FixieM f e a -> ([Note], Set Function,  Either e a)
notesFunctionsetOutputM = fixieM notesFunctionsetOutputT

notesFunctionsValueM :: f (FixieM f Void) -> FixieM f Void a -> ([Note], [Function], a)
notesFunctionsValueM = fixieM notesFunctionsValueT

notesFunctionsetValueM :: f (FixieM f Void) -> FixieM f Void a -> ([Note], Set Function, a)
notesFunctionsetValueM = fixieM notesFunctionsetValueT

functionsOutputNotesM :: f (FixieM f e) -> FixieM f e a -> ([Function], Either e a, [Note])
functionsOutputNotesM = fixieM functionsOutputNotesT

functionsValueNotesM :: f (FixieM f Void) -> FixieM f Void a -> ([Function], a, [Note])
functionsValueNotesM = fixieM functionsValueNotesT

functionsNotesOutputM :: f (FixieM f e) -> FixieM f e a -> ([Function], [Note], Either e a)
functionsNotesOutputM = fixieM functionsNotesOutputT

functionsNotesValueM :: f (FixieM f Void) -> FixieM f Void a -> ([Function], [Note], a)
functionsNotesValueM = fixieM functionsNotesValueT

functionsetOutputNotesM :: f (FixieM f e) -> FixieM f e a -> (Set Function, Either e a, [Note])
functionsetOutputNotesM = fixieM functionsetOutputNotesT

functionsetValueNotesM :: f (FixieM f Void) -> FixieM f Void a -> (Set Function, a, [Note])
functionsetValueNotesM = fixieM functionsetValueNotesT

functionsetNotesOutputM :: f (FixieM f e) -> FixieM f e a -> (Set Function, [Note], Either e a)
functionsetNotesOutputM = fixieM functionsetNotesOutputT

functionsetNotesValueM :: f (FixieM f Void) -> FixieM f Void a -> (Set Function, [Note], a)
functionsetNotesValueM = fixieM functionsetNotesValueT

outputNotesM :: f (FixieM f e) -> FixieM f e a -> (Either e a, [Note])
outputNotesM = fixieM outputNotesT

outputFunctionsM :: f (FixieM f e) -> FixieM f e a -> (Either e a, [Function])
outputFunctionsM = fixieM outputFunctionsT

outputFunctionsetM :: f (FixieM f e) -> FixieM f e a -> (Either e a, Set Function)
outputFunctionsetM = fixieM outputFunctionsetT

valueNotesM :: f (FixieM f Void) -> FixieM f Void a -> (a, [Note])
valueNotesM = fixieM valueNotesT

valueFunctionsM :: f (FixieM f Void) -> FixieM f Void a -> (a, [Function])
valueFunctionsM = fixieM valueFunctionsT

valueFunctionsetM :: f (FixieM f Void) -> FixieM f Void a -> (a, Set Function)
valueFunctionsetM = fixieM valueFunctionsetT

notesOutputM :: f (FixieM f e) -> FixieM f e a -> ([Note], Either e a)
notesOutputM = fixieM notesOutputT

notesValueM :: f (FixieM f Void) -> FixieM f Void a -> ([Note], a)
notesValueM = fixieM notesValueT

notesFunctionsM :: f (FixieM f e) -> FixieM f e a -> ([Note], [Function])
notesFunctionsM = fixieM notesFunctionsT

notesFunctionsetM :: f (FixieM f e) -> FixieM f e a -> ([Note], Set Function)
notesFunctionsetM = fixieM notesFunctionsetT

functionsOutputM :: f (FixieM f e) -> FixieM f e a -> ([Function], Either e a)
functionsOutputM = fixieM functionsOutputT

functionsValueM :: f (FixieM f Void) -> FixieM f Void a -> ([Function], a)
functionsValueM = fixieM functionsValueT

functionsNotesM :: f (FixieM f e) -> FixieM f e a -> ([Function], [Note])
functionsNotesM = fixieM functionsNotesT

functionsetOutputM :: f (FixieM f e) -> FixieM f e a -> (Set Function, Either e a)
functionsetOutputM = fixieM functionsetOutputT

functionsetValueM :: f (FixieM f Void) -> FixieM f Void a -> (Set Function, a)
functionsetValueM = fixieM functionsetValueT

functionsetNotesM :: f (FixieM f e) -> FixieM f e a -> (Set Function, [Note])
functionsetNotesM = fixieM functionsetNotesT

outputM :: f (FixieM f e) -> FixieM f e a -> (Either e a)
outputM = fixieM outputT

valueM :: f (FixieM f Void) -> FixieM f Void a -> a
valueM = fixieM valueT

notesM :: f (FixieM f e) -> FixieM f e a -> [Note]
notesM = fixieM notesT

functionsM :: f (FixieM f e) -> FixieM f e a -> [Function]
functionsM = fixieM functionsT

functionsetM :: f (FixieM f e) -> FixieM f e a -> (Set Function)
functionsetM = fixieM functionsetT
