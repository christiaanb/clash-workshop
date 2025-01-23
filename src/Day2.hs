module Day2 where

import Clash.Prelude

-- Exercise 1: calculate the average of the previous /n/ inputs. Use
-- 'register' and 'liftA' family of functions
movingAverage ::
  forall dom n .
  KnownDomain dom =>
  -- | Number of inputs to average
  SNat n ->
  Signal dom Int ->
  Signal dom Int
movingAverage howMany inp = avg
 where
  avg = undefined

-- Exercise 2: calculate the average of the previous /n/ inputs. Use the
-- 'mealy' or 'moore' function
movingAverageM ::
  forall dom n .
  -- | Number of inputs to average
  SNat n ->
  Signal dom Int ->
  Signal dom Int
movingAverageM howMany inp = avg
 where
  avg = undefined

-- Exercise 3: Count the number of 1's in a BitVector, one bit per clock cycle
--
-- You can assume the 'BitVector' remain stable as you sum the number of bits
bitsSetSerial ::
  forall n dom .
  (KnownDomain dom, KnownNat n) =>
  Signal dom (BitVector n) ->
  Signal dom (Maybe (Index (n+1)))
bitsSetSerial bv = cnt
 where
  cnt = undefined

-- Exercise 4: Create a circuit that recognizes a pattern in a stream, and
-- counts the number of occurances of this pattern.
--
-- Count the number of overlapping matches, i.e. in the stream "acacacaca" the
-- pattern "aca" should be recognized four times
countOverlapping ::
  forall dom n a .
  (KnownDomain dom, KnownNat n, 1 <= n, NFDataX a, Eq a) =>
  -- | The pattern to match against
  Vec n a ->
  -- | The incoming stream of values
  Signal dom a ->
  -- | The counted number of matched occurances so far
  Signal dom Int
countOverlapping pat inp = cnt
  where
   cnt = undefined

-- Exercise 4: Create a circuit that recognizes a pattern in a stream, and
-- counts the number of occurances of this pattern.
--
-- Count the number of non-overlapping matches, i.e. in the stream "acacacaca"
-- the pattern "aca" should be recognized two times
countNonOverlapping ::
  forall dom n a .
  (KnownDomain dom, KnownNat n, 1 <= n, NFDataX a, Eq a) =>
  -- | The pattern to match against
  Vec n a ->
  -- | The incoming stream of values
  Signal dom a ->
  -- | The counted number of matched occurances so far
  Signal dom Int
countNonOverlapping pat inp = cnt
  where
   cnt = undefined
