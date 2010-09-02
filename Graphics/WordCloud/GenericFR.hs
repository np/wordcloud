{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables, DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies  #-}

module Graphics.WordCloud.GenericFR (FRef(..), o, update) where

import qualified Control.Monad.State.Strict as S
import qualified Control.Exception as C
import Data.Dynamic
import Data.Generics
import Data.Typeable
import System.IO.Unsafe

newtype IntEx = IntEx Int deriving (Eq, Enum, Bounded, Show, Num, Typeable)

takeOne :: S.MonadState [a] m => m a
takeOne = do
  (x:xs) <- S.get
  S.put xs
  return x

getFRefIndex :: forall a b. (Data a) => (a -> b) -> Int
getFRefIndex ac = unsafePerformIO $ do
                           x <- C.try . C.evaluate . ac $ gb (undefined::a)
                           case x of
                             Left e -> case C.dynExceptions e >>= fromDynamic of
                                         Just (IntEx x) -> return x
                                         _ -> C.throw e
                             Right f -> error "FRef internal: this Either case should never happen. "
    where
      gb :: Data a => a -> a
      gb px = fst $ S.runState (fromConstrM gbuild' (head . dataTypeConstrs . dataTypeOf $ px)) [0..]
          where
            gbuild' :: forall c m. (Data c, S.MonadState [IntEx] m) => m c
            gbuild' = return . C.throwDyn =<< takeOne

updateFRefByIndex :: (Data r, Typeable a) => Int -> a -> r -> r
updateFRefByIndex i a r = fst $ S.runState (gmapM go r) 0
    where
      go :: Data d => d -> S.State Int d
      go d = do
        x <- S.get
        S.put (x+1)
        if x == i
          then case cast a of
                  Just a' -> return a'
                  _ -> error "FRef internal: something went very wrong."
          else return d

setter :: (Data a, Typeable b) => (a -> b) -> (b -> a -> a)
setter r = updateFRefByIndex (getFRefIndex r)

data FRefD s a = FRefD
    { get' :: s -> a
    , set' :: a -> s -> s }

class ToFRef r s a | r -> s a where
    toFRef :: r -> FRefD s a

class FRef r s a | r -> s a where
    get :: r -> s -> a
    set :: r -> a -> s -> s

instance ToFRef (FRefD s a) s a where
    toFRef = id

instance FRef (FRefD s a) s a where
    get = get'
    set = set'

instance (Data s, Typeable a) => ToFRef (s -> a) s a where
    toFRef x = FRefD { get' = x, set' = setter x }

instance (Data s, Typeable a) => FRef (s -> a) s a where
    get = id
    set = setter

o :: (ToFRef r b c, ToFRef r' a b) => r -> r' -> FRefD a c
o bc' ab' = FRefD
        { get' = get' bc    . get' ab
        , set' = update ab  . set' bc}
    where bc = toFRef bc'; ab = toFRef ab'

update :: (ToFRef r s a) => r -> (a -> a) -> (s -> s)
update ref' f s = set' ref (f (get' ref s)) s
    where ref = toFRef ref'
