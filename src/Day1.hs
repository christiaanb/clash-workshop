module Day1 where

import Clash.Prelude

-- Exercise 1: Create a halfadder
halfAdder :: Bit -> Bit -> (Bit,Bit)
halfAdder a b = (carry, summand)
 where
  carry   = undefined
  summand = undefined

-- Exercise 2: Create a fullAdder
fullAdder :: Bit -> (Bit,Bit) -> (Bit,Bit)
fullAdder carryIn (a,b) = (carryOut, summand)
 where
  carryOut = undefined
  summand  = undefined

-- Exercise 3: create a ripple carry adder
--
-- Hint: use 'unpack' to go from 'BitVector n' to 'Vec n Bit'
rippleAdder ::
  KnownNat n => Bit -> (BitVector n,BitVector n) -> (Bit, BitVector n)
rippleAdder carryIn (as,bs) = (carryOut, summand)
 where
  carryOut = undefined
  summand  = undefined

-- Exercise 4: Count the number of 1's in a BitVector
--
-- Hint: use 'unpack' to go from 'BitVector n' to 'Vec n (Unsigned 1)'
-- Hint: use 'resize' in combination with 'bitCoerce'
bitsSet :: forall n . KnownNat n => BitVector n -> Index (n+1)
bitsSet bv = cnt
 where
  cnt = undefined

-- Exercise 5: Determine the position of the first 1 in a BitVector
firstOne :: forall n . KnownNat n => BitVector n -> Maybe (Index n)
firstOne bv = pos
 where
  pos = undefined
