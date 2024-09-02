{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- Inspired on Data types a la carte

module Free.AlaCarte where

import Data.Kind

infixr 8 :+:

type (:+:) :: forall {k}. (k -> Type) -> (k -> Type) -> k -> Type
data (f :+: g) e = Left' (f e) | Right' (g e) deriving (Functor)

type (:<:) :: (Type -> Type) -> (Type -> Type) -> Constraint
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  {-# INLINEABLE inj #-}
  inj :: f a -> f a
  inj = id

  {-# INLINEABLE prj #-}
  prj :: f a -> Maybe (f a)
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  {-# INLINEABLE inj #-}
  inj :: f a -> (f :+: g) a
  inj = Left'

  {-# INLINEABLE prj #-}
  prj :: (f :+: g) a -> Maybe (f a)
  prj = \case
    Left' e -> Just e
    Right' _e -> Nothing

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, f :<: g, Functor h) => f :<: (h :+: g) where
  {-# INLINEABLE inj #-}
  inj :: f a -> (h :+: g) a
  inj = Right' . inj

  {-# INLINEABLE prj #-}
  prj :: (h :+: g) a -> Maybe (f a)
  prj = \case
    Left' _e -> Nothing
    Right' e -> prj e

type Free :: (Type -> Type) -> Type -> Type
data Free f a
  = Pure a
  | Impure (f (Free f a))
  deriving (Functor)

instance (Functor f) => Applicative (Free f) where
  {-# INLINEABLE pure #-}
  pure :: a -> Free f a
  pure = Pure

  {-# INLINEABLE (<*>) #-}
  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  Pure f <*> t = fmap f t
  Impure f <*> t = Impure (fmap (<*> t) f)

instance (Functor f) => Monad (Free f) where
  {-# INLINEABLE (>>=) #-}
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure x >>= f = f x
  Impure t >>= f = Impure (fmap (>>= f) t)

foldFree ::
  forall f a b.
  (Functor f) =>
  (a -> b) ->
  (f b -> b) ->
  Free f a ->
  b
foldFree pure' impure = go
  where
    go :: Free f a -> b
    go = \case
      Pure x -> pure' x
      Impure t -> impure (fmap go t)

exec :: (Exec f) => Free f a -> IO a
exec = foldFree return execAlgebra

instance (Exec f, Exec g) => Exec (f :+: g) where
  {-# INLINEABLE execAlgebra #-}
  execAlgebra = \case
    Left' e -> execAlgebra e
    Right' e -> execAlgebra e

type Exec :: (Type -> Type) -> Constraint
class (Functor f) => Exec f where
  execAlgebra :: f (IO a) -> IO a

injectFree :: (g :<: f) => g (Free f a) -> Free f a
injectFree = Impure . inj
