{-# LANGUAGE OverloadedStrings #-}

module Tests.Clash.Feistel.Ex2 where

import Hedgehog ((===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Prelude

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_aEqualsA :: H.Property
prop_aEqualsA = H.property $ do
  a <- H.forAll $ Gen.int (Range.linear 0 100)
  a === a

tests :: [TestTree]
tests =
  [ -- Your tests here
    testPropertyNamed "prop_aEqualsA" "prop_aEqualsA" prop_aEqualsA
  ]
