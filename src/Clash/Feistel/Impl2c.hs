{-# LANGUAGE RankNTypes #-}

{- |
Copyright: (C) 2020, QBayLogic B.V.
License:   see LICENSE
-}
module Clash.Feistel.Impl2c where

import Clash.Prelude hiding (round)

import qualified Clash.Signal.Delayed as D
import qualified Clash.Signal.Delayed.Bundle as D

import Clash.Feistel.Util (dfoldl)

{- | Specifies that 'wordSize' is twice the size of 'keySize', and that both
sizes are known at runtime.
-}
type SizeInvariants keySize wordSize =
  ( KnownNat keySize
  , KnownNat wordSize
  , wordSize ~ (2 * keySize)
  )

{- | A Feistel round function. Usually just called /F/ in literature. Takes a
/roundKey/ and /data/ to mangle.
-}
type RoundFunction domain roundDelay keySize =
  forall startDelay.
  Clock domain ->
  Reset domain ->
  Enable domain ->
  DSignal domain startDelay (BitVector keySize) ->
  DSignal domain startDelay (BitVector keySize) ->
  DSignal domain (startDelay + roundDelay) (BitVector keySize)

-- | Same as 'combine', but with a /hidden/ clock, reset, and enable.
combineI ::
  (KnownNat keySize, HiddenClockResetEnable domain) =>
  DSignal domain d (BitVector keySize) ->
  DSignal domain d (BitVector keySize) ->
  DSignal domain (d + 1) (BitVector keySize)
combineI key plain =
  D.delayedI 0 (liftA2 xor (plain * 16777619) key)

{- | Pseudorandom (with emphasis on pseudo) function to be used as Feistel
round function. First argument is the round /key/, second one the data
to be mangled.
-}
combine ::
  ( KnownDomain domain
  , KnownNat keySize
  ) =>
  RoundFunction domain 1 keySize
combine clk rst ena =
  withClockResetEnable clk rst ena combineI

-- | Split a word into two equal parts
splitWord :: (KnownNat n) => BitVector (n * 2) -> (BitVector n, BitVector n)
splitWord = split

-- | Executes single round of Feistel cipher with given round function
round ::
  ( HiddenClockResetEnable domain
  , SizeInvariants keySize wordSize
  , KnownNat roundDelay
  ) =>
  RoundFunction domain roundDelay keySize ->
  DSignal domain startDelay (BitVector wordSize) ->
  DSignal domain startDelay (BitVector keySize) ->
  DSignal domain (startDelay + roundDelay) (BitVector wordSize)
round roundFunction word key =
  let
    (l0, r0) = D.unbundle (splitWord <$> word)
    l0d = D.delayed (repeat 0) l0
    r0d = D.delayed (repeat 0) r0
   in
    liftA2 (++#) r0d (liftA2 xor l0d (hideClockResetEnable roundFunction key r0))

{- | Feistel cipher using given function as round function. Configurable in the
number of rounds.
-}
feistel ::
  ( HiddenClockResetEnable domain
  , SizeInvariants keySize wordSize
  , KnownNat rounds
  , KnownNat roundDelay
  ) =>
  -- | Round function
  RoundFunction domain roundDelay keySize ->
  -- | Round keys
  DSignal domain startDelay (Vec rounds (BitVector keySize)) ->
  -- | Plain text
  DSignal domain startDelay (BitVector wordSize) ->
  -- | Ciphertext
  DSignal domain (startDelay + rounds * roundDelay) (BitVector wordSize)
feistel roundFunction roundKeys plain =
  let
    -- 'dfold' delays the round keys equal to the delay /roundFunction/
    -- introduces
    lnrn = dfoldl (round roundFunction) 0 plain roundKeys
    (ln, rn) = D.unbundle (splitWord <$> lnrn)
   in
    -- The last round in a Feistel cipher is not supposed to swap its input
    -- words. 'round' does though, so we need to revert that here. Note that
    -- this comes at zero cost in hardware.
    liftA2 (++#) rn ln

-- Synthesizable version of Feistel network. Uses 'combine' as a round
-- function, and uses 5 rounds with a key size of 16 bits.
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  DSignal System 0 (Vec 5 (BitVector 16)) ->
  DSignal System 0 (BitVector 32) ->
  DSignal System 5 (BitVector 32)
topEntity clk rst ena =
  -- Due to https://github.com/clash-lang/clash-compiler/issues/1028 we
  -- currently can't use hidden constructs on top entities.
  withClockResetEnable clk rst ena (feistel combine)
