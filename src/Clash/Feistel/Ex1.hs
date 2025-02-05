{- |
Copyright: (C) 2020, QBayLogic B.V.
License:   see LICENSE
-}
module Clash.Feistel.Ex1 where

import Clash.Prelude

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
roundFunction :: (KnownNat n) => BitVector n -> BitVector n -> BitVector n
roundFunction key plain = (plain * 16777619) `xor` key

-- | Split a word into two equal parts
splitWord :: (KnownNat n) => BitVector (n * 2) -> (BitVector n, BitVector n)
splitWord = split

{- | Feistel cipher using 'roundFunction' as a round key. Configurable in the
number of rounds.
-}
feistel ::
  (SizeInvariants keySize wordSize) =>
  -- | Round keys
  Vec rounds (BitVector keySize) ->
  -- | Plain text
  BitVector wordSize ->
  -- | Ciphertext
  BitVector wordSize
feistel _roundKeys _plain =
  -- Exercise 1: Construct a Feistel network using 'roundFunction' as a
  -- round key. Information on the construction of Feistel networks can be
  -- found on Wikipedia:
  --
  --   https://en.wikipedia.org/wiki/Feistel_cipher#Construction_details
  --
  error "NYI"
