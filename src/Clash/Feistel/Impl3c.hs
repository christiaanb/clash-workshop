{- |
Copyright: (C) 2020, QBayLogic B.V.
License:   see LICENSE
-}
module Clash.Feistel.Impl3c where

import Clash.Prelude hiding (round)

data MangleMode
  = Encryption
  | Decryption
  deriving (Generic, NFDataX)

-- | Instruction for /F/
data FInstr nRounds keySize
  = -- | Load a round key into nth /F/ instruction machine
    LoadKey !(Index nRounds) !(BitVector keySize)
  | -- | Execute /F/ on given text
    Mangle !MangleMode !(BitVector (2 * keySize))
  | -- | Do nothing
    NoOp
  deriving (Generic, NFDataX)

emptyInstr :: FInstr nRounds keySize
emptyInstr = NoOp

data FState keySize = FState
  { encKey :: BitVector keySize
  , decKey :: BitVector keySize
  }
  deriving (Generic, NFDataX)

emptyState :: (KnownNat keySize) => FState keySize
emptyState = FState{encKey = 0, decKey = 0}

-- Gets either 'encKey' or 'decKey' from given FState, depending on given
-- MangleMode
getKey :: MangleMode -> FState keySize -> BitVector keySize
getKey Encryption = encKey
getKey Decryption = decKey

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
  ( SizeInvariants keySize wordSize
  , KnownNat nRounds
  ) =>
  RoundFunction keySize ->
  -- | Index of this /F/ instruction machine
  Index nRounds ->
  -- | State, currently just stores the key
  FState keySize ->
  -- | Instruction, either store a new key or mangle data
  FInstr nRounds keySize ->
  (FState keySize, FInstr nRounds keySize)
roundMachine roundFunction fIndex keys instr =
  case instr of
    NoOp ->
      (keys, instr)
    LoadKey iIndex key
      -- Update current key if this key is for us
      | iIndex == fIndex -> (keys{encKey = key}, instr)
      | iIndex == maxBound - fIndex -> (keys{decKey = key}, instr)
      | otherwise -> (keys, instr)
    Mangle mangleMode word ->
      -- Mangle given word, instruct next instruction machine to do the same
      let
        key = getKey mangleMode keys
        (l0, r0) = splitWord word
        mangled = r0 ++# (l0 `xor` roundFunction key r0)
       in
        (keys, Mangle mangleMode mangled)

-- Undelayed round
round ::
  ( HiddenClockResetEnable domain
  , KnownNat keySize
  , KnownNat nRounds
  ) =>
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
instrToResult NoOp = Nothing
instrToResult (LoadKey{}) = Nothing
instrToResult (Mangle _mangleMode word) =
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
