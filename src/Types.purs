module Types where

import Prelude
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Aff (Aff)
import Data.Lazy (Lazy, force, defer)
import Data.Exists (Exists)
import Data.CatList (CatList)
import Data.Tuple (Tuple(..))

--instance functorFetch :: Functor Fetch where
--  map f (Done a) = Done (f a)
--  map f (Blocked a) = Blocked (map (f <$> _) a)
--
--instance applyFetch :: Apply Fetch where
--  apply (Done f) (Done a) = Done (f a)
--  apply (Done f) (Blocked a) = Blocked (defer \_ -> (f <$> force a))
--  apply (Blocked f) (Done a) = Blocked (defer \_ -> (force f <*> Done a))
--  apply (Blocked f) (Blocked a) = Blocked (defer \_ -> (force f <*> force a))
--
--instance applicativeFetch :: Applicative Fetch where
--  pure = Done
--
--instance bindFetch :: Bind Fetch where
--  bind (Done m) f = f m
--  bind (Blocked m) f = Blocked $ defer \_ -> force m >>= f
--
--instance monadFetch :: Monad Fetch

data FetchStatus a =
    NotFetched
  | FetchSuccess a

data Request a

data BlockedRequest' a =
  BlockedRequest' (Request a) (Ref (FetchStatus a))

type BlockedRequest = Exists BlockedRequest'

data Result eff a =
    Done a
  | Blocked (CatList BlockedRequest) (Lazy (Fetch eff a))

instance functorResult :: Functor (Result eff) where
  map f (Done a) = Done (f a)
  map f (Blocked reqs a) = Blocked reqs (map (f <$> _) a)

newtype Fetch eff a = Fetch (Aff eff (Result eff a))

instance functorFetch :: Functor (Fetch eff) where
  map f (Fetch a) = Fetch $ map (f <$> _) a

instance applyFetch :: Apply (Fetch eff) where
  apply (Fetch f) (Fetch a) = Fetch $ do
    f' <- f
    a' <- a
    pure case Tuple f' a' of
      Tuple (Done g) (Done b) -> Done $ g b
      Tuple (Done g) (Blocked reqs b) -> Blocked reqs (map (g <$> _) b)
      Tuple (Blocked reqs g) (Done b) -> Blocked reqs (map ((_ $ b) <$> _) g)
      Tuple (Blocked greqs g) (Blocked breqs b) -> Blocked (greqs <> breqs) (defer \_ -> force g `apply` force b)

instance applicativeFetch :: Applicative (Fetch eff) where
  pure = Fetch <<< pure <<< Done

instance bindFetch :: Bind (Fetch eff) where
  bind (Fetch m) f = Fetch do
    m' <- m
    case m' of
      Done a -> case f a of
        Fetch a' -> a'
      Blocked reqs a -> pure $ Blocked reqs (defer \_ -> force a `bind` f)
