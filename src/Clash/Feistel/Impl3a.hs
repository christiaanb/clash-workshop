{- |
Copyright: (C) 2020, QBayLogic B.V.
License:   see LICENSE
-}
module Clash.Feistel.Impl3a where

import Clash.Prelude hiding (round)

-- | Instruction for /F/
data FInstr nRounds keySize
  = -- | Load a round key into nth /F/ instruction machine
    LoadKey !(Index nRounds) !(BitVector keySize)
  | -- | Execute /F/ on given text
    Mangle !(BitVector (2 * keySize))
  deriving (Generic, NFDataX)

emptyInstr :: (KnownNat keySize) => FInstr nRounds keySize
emptyInstr = Mangle 0

type FState keySize = BitVector keySize

emptyState :: (KnownNat keySize) => FState keySize
emptyState = 0

{- | Specifies that 'wordSize' is twice the size of 'keySize', and that both
sizes are known at runtime.
-}
type SizeInvariants keySize wordSize =
  ( KnownNat keySize
  , KnownNat wordSize
  , wordSize ~ (2 * keySize)
  )

{- | A Feistel round function. Usually just called /F/ in literature. Takes a
/roundKey/ and /data/ to mangle.
-}
type RoundFunction keySize =
  BitVector keySize ->
  BitVector keySize ->
  BitVector keySize

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
roundMachine ::
  (SizeInvariants keySize wordSize) =>
  RoundFunction keySize ->
  -- | Index of this /F/ instruction machine
  Index nRounds ->
  -- | State, currently just stores the key
  FState keySize ->
  -- | Instruction, either store a new key or mangle data
  FInstr nRounds keySize ->
  (FState keySize, FInstr nRounds keySize)
roundMachine roundFunction fIndex key0 instr =
  case instr of
    LoadKey iIndex key1
      -- Update current key if this key is for us
      | iIndex == fIndex -> (key1, instr)
      | otherwise -> (key0, instr)
    Mangle word ->
      -- Mangle given word, instruct next instruction machine to do the same
      let
        (l0, r0) = splitWord word
        mangled = r0 ++# (l0 `xor` roundFunction key0 r0)
       in
        (key0, Mangle mangled)

-- Undelayed round
round ::
  (HiddenClockResetEnable domain, KnownNat keySize) =>
  RoundFunction keySize ->
  Index nRounds ->
  Signal domain (FInstr nRounds keySize) ->
  Signal domain (FInstr nRounds keySize)
round roundFunction i =
  mealy (roundMachine roundFunction i) emptyState

instrToResult ::
  (KnownNat keySize) =>
  FInstr nRounds keySize ->
  Maybe (BitVector (2 * keySize))
instrToResult (LoadKey{}) = Nothing
instrToResult (Mangle word) =
  -- The last round in a Feistel cipher is not supposed to swap its input
  -- words. 'round' does though, so we need to revert that here. Note that
  -- this comes at zero cost in hardware.
  let (ln, rn) = splitWord word
   in Just (rn ++# ln)

{- | Feistel cipher using given function as round function. Configurable in the
number of rounds.
-}
feistel ::
  forall domain keySize wordSize nRounds.
  ( HiddenClockResetEnable domain
  , SizeInvariants keySize wordSize
  , KnownNat nRounds
  ) =>
  -- | Round function
  RoundFunction keySize ->
  -- | Input instructions
  Signal domain (FInstr nRounds keySize) ->
  {- | Results. "Just word" if last instruction machine mangled a word,
  "Nothing" otherwise.
  -}
  Signal domain (Maybe (BitVector wordSize))
feistel roundFunction instrs =
  let
    -- Given round function is suffixed with a single register.
    delayedRound inp n = register emptyInstr (round roundFunction n inp)
    delayedRounds = foldl delayedRound instrs (iterateI @nRounds (+ 1) 0)
   in
    instrToResult <$> delayedRounds

-- Synthesizable version of Feistel network. Uses 'combine' as a round
-- function, and uses 5 rounds with a key size of 16 bits.
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (FInstr 5 16) ->
  Signal System (Maybe (BitVector 32))
topEntity clk rst ena =
  -- Due to https://github.com/clash-lang/clash-compiler/issues/1028 we
  -- currently can't use hidden constructs on top entities.
  withClockResetEnable clk rst ena (feistel combine)
