module Data.OpenUnion
  ( OpenUnion()
  , Inject
  , inj
  , prj
  , TEQ, TCode(), HTrue(), HFalse(), Z(), S()
  ) where

import Prelude (Functor, map, unit)

import Data.Maybe (Maybe(..))

import Unsafe.Coerce (unsafeCoerce)

data OpenUnion a b w = H (a w) | T (b w)

data Z

data S n

data HTrue

data HFalse

class Inject e s where
  inj :: forall w. e w -> s w
  prj :: forall w. s w -> Maybe (e w)

instance injectOpenUnion :: (TEQ (TCode e) (TCode e1) HTrue, Includes b e e1 t) => Inject e (OpenUnion e1 t) where
  inj = inj' (unsafeCoerce unit :: b)
  prj = prj' (unsafeCoerce unit :: b)

instance functorOpenUnion :: (Functor s, Functor t) => Functor (OpenUnion s t) where
  map k (H w) = H (map k w)
  map k (T w) = T (map k w)

class Includes b e e1 s where
  inj' :: forall w. b -> e w -> OpenUnion e1 s w
  prj' :: forall w. b -> OpenUnion e1 s w -> Maybe (e w)

instance includesHTrue :: Includes HTrue e e t where
  inj' _ e = H e
  prj' _ (H e) = Just e
  prj' _ (T _) = Nothing

instance includesHFalse :: (Inject e t) => Includes HFalse e e1 t where
  inj' _ e = T (inj e)
  prj' _ (H e) = Nothing
  prj' _ (T e) = prj e

class TEQ n1 n2 teq

instance teqZZ :: TEQ Z Z HTrue

instance teqSnZ :: TEQ (S n) Z HFalse

instance teqZSn :: TEQ Z (S n) HFalse

instance teqSnSn :: (TEQ n1 n2 v) => TEQ (S n1) (S n2) v

data TCode (n :: * -> *)
