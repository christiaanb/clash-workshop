{- |
Copyright: (C) 2020, QBayLogic B.V.
License:   see LICENSE
-}
module Clash.Feistel.Impl1b where

import Clash.Prelude hiding (round)

{- | Specifies that 'wordSize' is twice the size of 'keySize', and that both
sizes are known at runtime.
-}
type SizeInvariants keySize wordSize =
  ( KnownNat keySize
  , KnownNat wordSize
  , wordSize ~ (2 * keySize)
  )

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
  (BitVector keySize -> BitVector keySize -> BitVector keySize) ->
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
  (SizeInvariants keySize wordSize) =>
  -- | Round function
  (BitVector keySize -> BitVector keySize -> BitVector keySize) ->
  -- | Round keys
  Vec rounds (BitVector keySize) ->
  -- | Plain text
  BitVector wordSize ->
  -- | Ciphertext
  BitVector wordSize
feistel roundFunction roundKeys plain =
  let
    lnrn = foldl (round roundFunction) plain roundKeys
    (ln, rn) = splitWord lnrn
   in
    -- The last round in a Feistel cipher is not supposed to swap its input
    -- words. 'round' does though, so we need to revert that here. Note that
    -- this comes at zero cost in hardware.
    rn ++# ln
