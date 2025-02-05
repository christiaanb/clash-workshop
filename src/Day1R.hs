module Day1 where

import Clash.Prelude

-- Exercise 1: Create a halfadder
halfAdder :: Bit -> Bit -> (Bit,Bit)
halfAdder a b = (carry, summand)
 where
  carry   = a .&. b
  summand = xor a b

-- Exercise 2: Create a fullAdder
fullAdder :: Bit -> (Bit,Bit) -> (Bit,Bit)
fullAdder carryIn (a,b) = (carryOut, summand)
 where
  (carry1,partialSum) = halfAdder a b
  (carry2,summand)    = halfAdder partialSum carryIn
  carryOut            = carry1 .|. carry2

-- Exercise 3: create a ripple carry adder
--
-- Hint: use 'unpack' to go from 'BitVector n' to 'Vec n Bit'
rippleAdder ::
  KnownNat n => Bit -> (BitVector n,BitVector n) -> (Bit, BitVector n)
rippleAdder carryIn (as,bs) = (carryOut, summand)
 where
  (carryOut,summandV) = mapAccumR fullAdder carryIn (zip (unpack as) (unpack bs))
  summand             = pack summandV

-- Exercise 4: Count the number of 1's in a BitVector
--
-- Hint: use 'unpack' to go from 'BitVector n' to 'Vec n (Unsigned 1)'
-- Hint: use 'resize' in combination with 'bitCoerce'
bitsSet :: forall n . KnownNat n => BitVector n -> Index (n+1)
bitsSet bv = sum uVs1
  where
    uVs :: Vec n (Unsigned 1)
    uVs = bitCoerce bv

    uVs1 :: Vec n (Index (n + 1))
    uVs1 = map (bitCoerce . resize) uVs

-- Exercise 5: Determine the position of the first 1 in a BitVector
firstOne :: forall n . KnownNat n => BitVector n -> Maybe (Index n)
firstOne bv = pos
 where
  pos = ifoldr find Nothing (unpack bv)

  find idx b p
    | b = Just (maxBound - idx)
    | otherwise = p
