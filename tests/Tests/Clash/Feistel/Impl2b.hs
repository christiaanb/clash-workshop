module Tests.Clash.Feistel.Impl2b where

import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Prelude
import Hedgehog ((/==), (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Clash.Feistel.Impl1c as FPrev
import qualified Clash.Feistel.Impl2b as F
import qualified Tests.Clash.Feistel.Impl1 as Impl1

{- | Same as F.feistel, but works on lists, and eliminates need to create clocks,
resets, and enables. Asserts reset for a single cycle.
-}
feistel' ::
  forall rounds keySize wordSize.
  ( F.SizeInvariants keySize wordSize
  , KnownNat rounds
  ) =>
  Vec rounds (BitVector keySize) ->
  [BitVector wordSize] ->
  [BitVector wordSize]
feistel' keys plains0 =
  L.drop prelude
    $ sampleN
      @System
      (prelude + L.length plains0)
      (F.feistel F.combine (pure keys) plains1)
 where
  prelude = snatToNum (SNat @rounds) + 1
  plains1 = fromList (0 : plains0 <> L.repeat 0)

genPlains :: (KnownNat keySize) => H.Gen [BitVector (2 * keySize)]
genPlains = Gen.list (Range.linear 1 100) genDefinedBitVector

-- | Test whether encryption can be reversed by decryption
testId :: forall rounds keySize. SNat rounds -> SNat keySize -> H.Property
testId SNat SNat = H.property $ do
  keys <- H.forAll $ Impl1.genKeys @rounds @keySize
  plains <- H.forAll $ genPlains @keySize

  let
    encs = feistel' keys plains
    decs = feistel' (reverse keys) encs

  plains === decs

testIdWithImpl1 :: forall rounds keySize. SNat rounds -> SNat keySize -> H.Property
testIdWithImpl1 SNat SNat = H.property $ do
  keys <- H.forAll $ Impl1.genKeys @rounds @keySize
  plains <- H.forAll $ genPlains @keySize

  let
    ciphers1 = L.map (FPrev.feistel F.combine keys) plains
    ciphers2b = feistel' keys plains

  ciphers1 === ciphers2b

{- | Test whether an encryption run of some plaintext does not yield the
plaintext. Can be expected to be true for large key/word sizes and a
non-zero number of rounds.
-}
testNotId :: forall rounds keySize. SNat rounds -> SNat keySize -> H.Property
testNotId SNat SNat = H.property $ do
  keys <- H.forAll $ Impl1.genKeys @rounds @keySize
  plains <- H.forAll $ genPlains @keySize
  plains /== feistel' keys plains

tests :: [TestTree]
tests =
  [ testProperty "id, rounds=0, keySize=16" (testId (SNat @0) (SNat @16))
  , testProperty "id, rounds=1, keySize=16" (testId (SNat @1) (SNat @16))
  , testProperty "id, rounds=16, keySize=16" (testId (SNat @16) (SNat @16))
  , testProperty "id, rounds=0, keySize=0" (testId (SNat @0) (SNat @0))
  , testProperty "id, rounds=1, keySize=0" (testId (SNat @1) (SNat @0))
  , testProperty "id, rounds=16, keySize=0" (testId (SNat @16) (SNat @0))
  , testProperty "id with impl1, rounds=0, keySize=16" (testIdWithImpl1 (SNat @0) (SNat @16))
  , testProperty "id with impl1, rounds=1, keySize=16" (testIdWithImpl1 (SNat @1) (SNat @16))
  , testProperty "id with impl1, rounds=16, keySize=16" (testIdWithImpl1 (SNat @16) (SNat @16))
  , testProperty "id with impl1, rounds=0, keySize=0" (testIdWithImpl1 (SNat @0) (SNat @0))
  , testProperty "id with impl1, rounds=1, keySize=0" (testIdWithImpl1 (SNat @1) (SNat @0))
  , testProperty "id with impl1, rounds=16, keySize=0" (testIdWithImpl1 (SNat @16) (SNat @0))
  , testProperty "notId, rounds=5, keySize=128" (testNotId (SNat @5) (SNat @128))
  ]
