module Day2R where

import Clash.Prelude

type Mealy s i o = s -> i -> (s,o)

-- Exercise 1: calculate the average of the previous /n/ inputs. Use
-- 'register' and 'liftA' family of functions
movingAverage ::
  forall dom n .
  HiddenClockResetEnable dom =>
  -- | Number of inputs to average
  SNat n ->
  Signal dom Int ->
  Signal dom Int
movingAverage howMany@SNat inp = avg
 where
  avg = fmap (`div` snatToNum howMany) (sum w)
  w   = iterate howMany (register 0) inp

-- Exercise 2: calculate the average of the previous /n/ inputs. Use the
-- 'mealy' or 'moore' function
movingAverageM ::
  forall dom n .
  (HiddenClockResetEnable dom, KnownNat n) =>
  -- | Number of inputs to average
  SNat n ->
  Signal dom Int ->
  Signal dom Int
movingAverageM howMany inp = avg
 where
  avg = mealy go (repeat 0) inp

  go :: Mealy (Vec n Int) Int Int
  go w i = (wN, sum wN `div` snatToNum howMany)
    where
      wN = init (i :> w)

-- Exercise 3: Count the number of 1's in a BitVector, one bit per clock cycle
--
-- You can assume the 'BitVector' remain stable as you sum the number of bits
bitsSetSerial ::
  forall n dom .
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom (BitVector n) ->
  Signal dom (Maybe (Index (n+1)))
bitsSetSerial = mealy bitCnt (0,0)

bitCnt ::
  KnownNat n =>
  Mealy (Index (n+1), Index n)
        (BitVector n)
        (Maybe (Index (n + 1)))
bitCnt (cnt,idx) bv = ((cntN2,idxN),outp)
 where
  cntN | bv ! idx == high = cnt + 1
       | otherwise        = cnt
  idxN | idx == maxBound  = minBound
       | otherwise        = idx + 1

  cntN2 | idx == maxBound = 0
        | otherwise       = cntN

  outp
    | idx == maxBound = Just cntN
    | otherwise       = Nothing

-- Exercise 4: Create a circuit that recognizes a pattern in a stream, and
-- counts the number of occurances of this pattern.
--
-- Count the number of overlapping matches, i.e. in the stream "acacacaca" the
-- pattern "aca" should be recognized four times
countOverlapping ::
  forall dom n a .
  (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX a, Eq a) =>
  -- | The pattern to match against
  Vec n a ->
  -- | The incoming stream of values
  Signal dom a ->
  -- | The counted number of matched occurances so far
  Signal dom Int
countOverlapping pat = mealy match (pat,0)
 where
  match :: Mealy (Vec n a,Int) a Int
  match (win,cnt) i = ((winN,cntN), cntN)
    where
      winN = tail (win :< i)
      cntN | pat == winN = cnt + 1
           | otherwise   = cnt

-- Exercise 4: Create a circuit that recognizes a pattern in a stream, and
-- counts the number of occurances of this pattern.
--
-- Count the number of non-overlapping matches, i.e. in the stream "acacacaca"
-- the pattern "aca" should be recognized two times
countNonOverlapping ::
  forall dom n a .
  (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX a, Eq a) =>
  -- | The pattern to match against
  Vec n a ->
  -- | The incoming stream of values
  Signal dom a ->
  -- | The counted number of matched occurances so far
  Signal dom Int
countNonOverlapping pat = mealy match (pat,0,maxBound)
 where
  match :: Mealy (Vec n a, Int, Index n) a Int
  match (win,cnt,prevMatchCnt) i = ((winN,cntN,prevMatchCntN),cntN)
    where
      winN = tail (win :< i)

      newMatch = pat == winN && prevMatchCnt == maxBound

      cntN | newMatch  = cnt + 1
           | otherwise = cnt

      prevMatchCntN | newMatch                 = 0
                    | prevMatchCnt == maxBound = prevMatchCnt
                    | otherwise                = prevMatchCnt + 1
