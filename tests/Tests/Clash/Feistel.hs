module Tests.Clash.Feistel where

import Test.Tasty
import Prelude

import qualified Tests.Clash.Feistel.Ex1
import qualified Tests.Clash.Feistel.Ex2
import qualified Tests.Clash.Feistel.Impl1
import qualified Tests.Clash.Feistel.Impl2b

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Clash.Feistel"
    [ testGroup "Impl1" Tests.Clash.Feistel.Impl1.tests
    , testGroup "Ex1" Tests.Clash.Feistel.Ex1.tests
    , testGroup "Impl2b" Tests.Clash.Feistel.Impl2b.tests
    , testGroup "Ex2" Tests.Clash.Feistel.Ex2.tests
    ]
