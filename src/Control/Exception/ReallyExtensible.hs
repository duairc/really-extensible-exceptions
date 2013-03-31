{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Exception.ReallyExtensible
    ( Some
    , toExtensibleException
    , fromExtensibleException
    , Cast
    , catchableAs
    )
where

import           Control.Exception (SomeException (SomeException))
import qualified Control.Exception as E
                     ( Exception (toException, fromException)
                     )
import           Control.Monad (msum)
import           Data.Dynamic (Dynamic, toDyn, fromDynamic)
import           Data.Typeable (Typeable, cast)


------------------------------------------------------------------------------
data Some c where
    Some :: forall a c. (c a, E.Exception a) => [Dynamic] -> a -> Some c
  deriving Typeable


------------------------------------------------------------------------------
instance Show (Some c) where
    showsPrec p (Some _ a) = showsPrec p a


------------------------------------------------------------------------------
instance Typeable c => E.Exception (Some c) where
    toException (Some es s) = SomeException (Some es s :: Some Exception)
    fromException (SomeException e) = do
        Some es _ <- cast e :: Maybe (Some Exception)
        msum $ map fromDynamic es


------------------------------------------------------------------------------
toExtensibleException :: E.Exception e => [Cast e] -> e -> SomeException
toExtensibleException fs e = SomeException
    (Some (let es = map (\(Cast f) -> f es e) fs in es) e :: Some Exception)


------------------------------------------------------------------------------
fromExtensibleException :: E.Exception e => SomeException -> Maybe e
fromExtensibleException (SomeException e) = do
    Some _ e' <- cast e :: Maybe (Some Exception)
    cast e'


------------------------------------------------------------------------------
newtype Cast e = Cast ([Dynamic] -> e -> Dynamic)


------------------------------------------------------------------------------
catchableAs :: (Typeable c, c e, E.Exception e) => proxy c -> Cast e
catchableAs p = Cast $ \es e -> toDyn $ fromProxy p es e
  where
    fromProxy :: (Typeable c, c a, E.Exception a)
        => proxy c -> [Dynamic] -> a -> Some c
    fromProxy _ = Some


------------------------------------------------------------------------------
-- We would just use 'E.Exception' directly, except it is not an instance of
-- Typeable and we don't want to export an orphan instance.
class E.Exception a => Exception a
deriving instance Typeable Exception
instance E.Exception a => Exception a