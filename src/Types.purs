module Types where

import Prelude
import Control.Monad.Eff.Ref (Ref, newRef, readRef)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Lazy (Lazy, force, defer)
import Data.Exists (Exists, mkExists)
import Data.CatList (CatList, cons, empty)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-- | FetchStatus results the status of a data retrieval,
-- | a binary state of `NotFetched` or the result.
data FetchStatus a =
    NotFetched
  | FetchSuccess a

-- | This is application specific, could be a web3 request or ajax request etc.
data Request a

-- | A `BlockedRequest` is a request together with a ref that will be updated when the
-- | request's return value when it is run.
data BlockedRequest' a =
  BlockedRequest' (Request a) (Ref (FetchStatus a))

type BlockedRequest = Exists BlockedRequest'

-- | This is the result type of a data retrieval -- either the data retrieval is
-- | `Done`, or there is a list of `BlockedRequest`s of various types that need to
-- | be run before we can continue the data retrieval.
data Result eff a =
    Done a
  | Blocked (CatList BlockedRequest) (Lazy (Fetch eff a))

instance functorResult :: Functor (Result eff) where
  map f (Done a) = Done (f a)
  map f (Blocked reqs a) = Blocked reqs (map (f <$> _) a)

-- | Our data retrieval type, an IO computation ending in a `Result`.
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

-- | `dataFetch` takes a normal request and turns it into a haxl friendly request,
-- | i.e. a `Fetch`
dataFetch
  :: forall eff a.
     Request a
  -> Fetch eff a
dataFetch req = Fetch $ do
  box <- liftEff <<< unsafeCoerceEff $ newRef NotFetched
  let br = mkExists $ BlockedRequest' req box
      cont = defer $ \_ -> Fetch $ do
        res <- liftEff <<< unsafeCoerceEff $ readRef box
        unsafePartial $ case res of
          FetchSuccess a -> pure $ Done a
  pure $ Blocked (singleton br) cont


singleton
  :: forall a.
     a
  -> CatList a
singleton a = cons a empty
