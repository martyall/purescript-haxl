module Types where

import Prelude
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Lazy (Lazy, force, defer)
import Data.Maybe (Maybe(..))
import Data.Exists (Exists, mkExists)
import Data.CatList (CatList, cons, empty)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.StrMap as M
import Unsafe.Coerce (unsafeCoerce)

-- | This is application specific, could be a web3 request or ajax request etc.
data Request a

--------------------------------------------------------------------------------
-- | Cache
--------------------------------------------------------------------------------

newtype DataCache = DataCache (M.StrMap (Ref (Exists FetchStatus)))

class Hashable a where
  hash :: a -> String

instance hashRequest :: Hashable (Request a) where
  hash _ = unsafeCrashWith "Hashable not implemented for Request types."

lookup
  :: forall a.
     Request a
  -> DataCache
  -> Maybe (Ref (FetchStatus a))
lookup req (DataCache cache) =
  unsafeCoerce <$> M.lookup (hash req) cache

insert
  :: forall a.
     Request a
  -> Ref (FetchStatus a)
  -> DataCache
  -> DataCache
insert req ref (DataCache cache) =
  DataCache $ M.insert (hash req) (unsafeCoerce ref) cache

--------------------------------------------------------------------------------
-- | Fetch
--------------------------------------------------------------------------------

-- | FetchStatus results the status of a data retrieval,
-- | a binary state of `NotFetched` or the result.
data FetchStatus a =
    NotFetched
  | FetchSuccess a
  | FetchFailure Error

-- | A `BlockedRequest` is a request together with a ref that will be updated when the
-- | request's return value when it is run.
data BlockedRequest' a =
  BlockedRequest' (Request a) (Ref (FetchStatus a))

type BlockedRequest = Exists BlockedRequest'

data Result eff a =
    Done a
  | Throw Error
  | Blocked (CatList BlockedRequest) (Lazy (Fetch eff a))

instance functorResult :: Functor (Result eff) where
  map f (Done a) = Done (f a)
  map f (Throw err) = Throw err
  map f (Blocked reqs a) = Blocked reqs (map (f <$> _) a)

-- | Our data retrieval type, an IO computation ending in a `Result`.
newtype Fetch eff a = Fetch (ReaderT (Ref DataCache) (Aff eff) (Result eff a))

instance functorFetch :: Functor (Fetch eff) where
  map f (Fetch a) = Fetch $ map (f <$> _) a

instance applyFetch :: Apply (Fetch eff) where
  apply (Fetch f) (Fetch a) = Fetch $ do
    f' <- f
    a' <- a
    pure case Tuple f' a' of
      Tuple (Done g) (Done b) -> Done $ g b
      Tuple (Done g) (Blocked breqs b) -> Blocked breqs (map (g <$> _) b)
      Tuple (Done _) (Throw err) -> Throw err
      Tuple (Blocked greqs g) (Done b) -> Blocked greqs (map ((_ $ b) <$> _) g)
      Tuple (Blocked greqs g) (Blocked breqs b) -> Blocked (greqs <> breqs) (defer \_ -> force g `apply` force b)
      Tuple (Blocked greqs g) (Throw err) -> Blocked greqs (defer \_ -> force g <*> throw err)
      Tuple (Throw err) _ -> Throw err

instance applicativeFetch :: Applicative (Fetch eff) where
  pure = Fetch <<< pure <<< Done

instance bindFetch :: Bind (Fetch eff) where
  bind (Fetch m) f = Fetch do
    m' <- m
    case m' of
      Done a -> case f a of
        Fetch a' -> a'
      Blocked reqs a -> pure $ Blocked reqs (defer \_ -> force a `bind` f)
      Throw err -> pure $ Throw err

-- | `dataFetch` takes a normal request and turns it into a haxl friendly request,
-- | i.e. a `Fetch`
dataFetch
  :: forall eff a.
     Request a
  -> Fetch eff a
dataFetch req = Fetch $ ReaderT $ \cacheRef -> do
    cache <- liftEff <<< unsafeCoerce $ readRef cacheRef
    case lookup req cache of
      Nothing -> do
        box <- liftEff <<< unsafeCoerceEff $ newRef NotFetched
        _ <- liftEff <<< unsafeCoerce $ writeRef cacheRef (insert req box cache)
        let br = mkExists $ BlockedRequest' req box
        pure $ Blocked (singleton br) (cont box)
      Just box -> do
        r <- liftEff <<< unsafeCoerce $ readRef box
        case r of
          FetchSuccess result -> pure $ Done result
          FetchFailure err -> pure $ Throw err
          NotFetched -> pure $ Blocked empty (cont box)
  where
    cont box = defer $ \_ -> Fetch $ ReaderT $ \cacheRef -> do
      res <- liftEff <<< unsafeCoerceEff $ readRef box
      unsafePartial $ case res of
        FetchSuccess a -> pure $ Done a

throw
  :: forall eff a.
     Error
  -> Fetch eff a
throw = Fetch <<< pure <<< Throw


catch
  :: forall eff a.
     Fetch eff a
  -> (Error -> Fetch eff a)
  -> Fetch eff a
catch (Fetch a) handler = Fetch $ ReaderT \cache -> do
  a' <- runReaderT a cache
  case a' of
    Done result -> pure $ Done result
    Throw err -> case handler err of
      Fetch b -> runReaderT b cache
    Blocked br b -> pure $ Blocked br (defer \_ -> catch (force b) handler)

singleton
  :: forall a.
     a
  -> CatList a
singleton a = cons a empty
