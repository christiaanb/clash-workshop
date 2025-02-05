{-# LANGUAGE RankNTypes #-}

{- |
Copyright: (C) 2020, QBayLogic B.V.
License:   see LICENSE
-}
module Clash.Feistel.Util (
  dfoldl,
  delayByN,
) where

import Clash.Prelude

import qualified Clash.Signal.Delayed as D

{- | Delay a signal by /n/ cycles. Example usage:

>>> delayByN (SNat @n) 3 inp
-}
delayByN ::
  forall n a dom.
  ( HiddenClockResetEnable dom
  , NFDataX a
  ) =>
  SNat n ->
  a ->
  Signal dom a ->
  Signal dom a
delayByN SNat aInit =
  foldl (.) id (repeat @n (register aInit))

{- | 'dfoldl' is like 'foldl', but it takes a a function that introduces a
delay. 'dfoldl' will add the appropriate delay for the elements of the
given vector.
-}
dfoldl ::
  forall domain b a n fDelay startDelay.
  ( HiddenClockResetEnable domain
  , NFDataX a
  , KnownNat n
  , KnownNat fDelay
  ) =>
  -- | Function to fold with introducing 'fDelay' cycles of delay
  ( forall s.
    DSignal domain s b ->
    DSignal domain s a ->
    DSignal domain (s + fDelay) b
  ) ->
  -- | Initial value of registers storing 'a'
  a ->
  -- | Start value
  DSignal domain startDelay b ->
  -- | Vector to fold over
  DSignal domain startDelay (Vec n a) ->
  -- | Fold result. Only valid n*fDelay cycles later.
  DSignal domain (startDelay + n * fDelay) b
dfoldl f0 aInit start vec0 =
  D.unsafeFromSignal (foldl f1 (D.toSignal start) vec1)
 where
  regN = delayByN (SNat @fDelay) aInit
  vec1 = delays (unbundle (D.toSignal vec0))
  delays v = snd (mapAccumL (\reg a -> (regN . reg, reg a)) id v)
  f1 b a = D.toSignal (f0 (D.fromSignal b) (D.fromSignal a))
