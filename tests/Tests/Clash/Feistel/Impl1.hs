module Tests.Clash.Feistel.Impl1 where

import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Prelude
import Hedgehog ((/==), (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import qualified Hedgehog as H

import qualified Clash.Feistel.Impl1c as F

genPlain :: (KnownNat keySize) => H.Gen (BitVector (2 * keySize))
genPlain = genDefinedBitVector

genKeys :: (KnownNat rounds, KnownNat keySize) => H.Gen (Vec rounds (BitVector keySize))
genKeys = genVec genDefinedBitVector

-- | Test whether encryption can be reversed by decryption
testId :: forall rounds keySize. SNat rounds -> SNat keySize -> H.Property
testId SNat SNat = H.property $ do
  keys <- H.forAll $ genKeys @rounds @keySize
  plain <- H.forAll $ genPlain @keySize

  let
    planEnc = F.feistel F.combine keys plain
    planDec = F.feistel F.combine (reverse keys) planEnc

  planDec === plain

{- | Test whether an encryption run of some plaintext does not yield the
plaintext. Can be expected to be true for large key/word sizes and a
non-zero number of rounds.
-}
testNotId :: forall rounds keySize. SNat rounds -> SNat keySize -> H.Property
testNotId SNat SNat = H.property $ do
  keys <- H.forAll $ genKeys @rounds @keySize
  plain <- H.forAll $ genPlain @keySize

  let planEnc = F.feistel F.combine keys plain

  planEnc /== plain

tests :: [TestTree]
tests =
  [ testProperty "id, rounds=0, keySize=16" (testId (SNat @0) (SNat @16))
  , testProperty "id, rounds=1, keySize=16" (testId (SNat @1) (SNat @16))
  , testProperty "id, rounds=16, keySize=16" (testId (SNat @16) (SNat @16))
  , testProperty "id, rounds=0, keySize=0" (testId (SNat @0) (SNat @0))
  , testProperty "id, rounds=1, keySize=0" (testId (SNat @1) (SNat @0))
  , testProperty "id, rounds=16, keySize=0" (testId (SNat @16) (SNat @0))
  , testProperty "notId, rounds=5, keySize=128" (testNotId (SNat @5) (SNat @128))
  ]
