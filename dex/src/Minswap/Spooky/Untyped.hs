{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Minswap.Spooky.Untyped
  ( Spooky,
    toSpooky,
    unSpooky,
    mapSpooky,
    liftSpooky,
    liftSpooky2,
    liftSpooky3,
    liftSpooky4,
    liftSpooky5,
  )
where

import PlutusTx
  ( BuiltinData,
    ToData,
    UnsafeFromData,
    toBuiltinData,
    unsafeFromBuiltinData,
  )

type Spooky a = BuiltinData

{-# INLINEABLE unSpooky #-}
unSpooky ::
  UnsafeFromData a =>
  Spooky a ->
  a
unSpooky = unsafeFromBuiltinData

toSpooky ::
  ToData a =>
  a ->
  Spooky a
toSpooky = toBuiltinData
{-# INLINE toSpooky #-}

liftSpooky ::
  (UnsafeFromData a) =>
  (a -> b) ->
  Spooky a ->
  b
liftSpooky f = f . unSpooky
{-# INLINE liftSpooky #-}

liftSpooky2 ::
  ( UnsafeFromData a,
    UnsafeFromData b
  ) =>
  (a -> b -> c) ->
  Spooky a ->
  Spooky b ->
  c
liftSpooky2 f a b =
  f (unSpooky a) (unSpooky b)
{-# INLINE liftSpooky2 #-}

liftSpooky3 ::
  ( UnsafeFromData a,
    UnsafeFromData b,
    UnsafeFromData c
  ) =>
  (a -> b -> c -> d) ->
  Spooky a ->
  Spooky b ->
  Spooky c ->
  d
liftSpooky3 f a b c =
  f (unSpooky a) (unSpooky b) (unSpooky c)
{-# INLINE liftSpooky3 #-}

liftSpooky4 ::
  ( UnsafeFromData a,
    UnsafeFromData b,
    UnsafeFromData c,
    UnsafeFromData d
  ) =>
  (a -> b -> c -> d -> e) ->
  Spooky a ->
  Spooky b ->
  Spooky c ->
  Spooky d ->
  e
liftSpooky4 f a b c d =
  f (unSpooky a) (unSpooky b) (unSpooky c) (unSpooky d)
{-# INLINE liftSpooky4 #-}

liftSpooky5 ::
  ( UnsafeFromData a,
    UnsafeFromData b,
    UnsafeFromData c,
    UnsafeFromData d,
    UnsafeFromData e
  ) =>
  (a -> b -> c -> d -> e -> f) ->
  Spooky a ->
  Spooky b ->
  Spooky c ->
  Spooky d ->
  Spooky e ->
  f
liftSpooky5 f a b c d e =
  f (unSpooky a) (unSpooky b) (unSpooky c) (unSpooky d) (unSpooky e)
{-# INLINE liftSpooky5 #-}

mapSpooky ::
  (UnsafeFromData a, ToData b) =>
  (a -> b) ->
  Spooky a ->
  Spooky b
mapSpooky f = toSpooky . f . unSpooky
{-# INLINE mapSpooky #-}
