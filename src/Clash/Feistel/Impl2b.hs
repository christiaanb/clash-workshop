{- |
Copyright: (C) 2020, QBayLogic B.V.
License:   see LICENSE
-}
module Clash.Feistel.Impl2b where

import Clash.Prelude hiding (round)

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
type RoundFunction keySize =
  BitVector keySize ->
  BitVector keySize ->
  BitVector keySize

{- | Pseudorandom (with emphasis on pseudo) function to be used as Feistel
round function. First argument is the round /key/, second one the data
to be mangled.
-}
combine :: (KnownNat n) => BitVector n -> BitVector n -> BitVector n
combine key plain = (plain * 16777619) `xor` key

-- | Split a word into two equal parts
splitWord :: (KnownNat n) => BitVector (n * 2) -> (BitVector n, BitVector n)
splitWord = split

-- | Executes single round of Feistel cipher with given round function
round ::
  (SizeInvariants keySize wordSize) =>
  RoundFunction keySize ->
  BitVector wordSize ->
  BitVector keySize ->
  BitVector wordSize
round roundFunction word key =
  let (l0, r0) = splitWord word
   in r0 ++# (l0 `xor` roundFunction key r0)

{- | Feistel cipher using given function as round function. Configurable in the
number of rounds.
-}
feistel ::
  ( HiddenClockResetEnable domain
  , SizeInvariants keySize wordSize
  , KnownNat rounds
  ) =>
  -- | Round function
  RoundFunction keySize ->
  -- | Round keys
  Signal domain (Vec rounds (BitVector keySize)) ->
  -- | Plain text
  Signal domain (BitVector wordSize) ->
  -- | Ciphertext
  Signal domain (BitVector wordSize)
feistel roundFunction roundKeys plain =
  let
    -- Given round function is suffixed with a single register.
    delayedRound word key = register 0 (liftA2 (round roundFunction) word key)
    lnrn = foldl delayedRound plain (unbundle roundKeys)
    (ln, rn) = unbundle (splitWord <$> lnrn)
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
  Signal System (Vec 5 (BitVector 16)) ->
  Signal System (BitVector 32) ->
  Signal System (BitVector 32)
topEntity clk rst ena =
  -- Due to https://github.com/clash-lang/clash-compiler/issues/1028 we
  -- currently can't use hidden constructs on top entities.
  withClockResetEnable clk rst ena (feistel combine)
