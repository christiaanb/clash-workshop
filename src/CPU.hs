{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

module CPU where

import Clash.Prelude
import Clash.Prelude.Moore (medvedev)
import Data.Maybe (fromMaybe)


type InstrAddr = Unsigned 8
type MemAddr   = Unsigned 5
type Value     = Signed 8

data Instruction
  = Compute Operator Reg Reg Reg
  | Branch Reg Value
  | Jump Value
  | Load MemAddr Reg
  | Store Reg MemAddr
  | Nop
  deriving (Eq, Show, Generic, NFDataX)

data Reg
  = Zero
  | PC
  | RegA
  | RegB
  | RegC
  | RegD
  | RegE
  deriving (Eq, Show, Enum, Generic, NFDataX)

data Operator = Add | Sub | Incr | Imm | CmpGt
  deriving (Eq, Show, Generic, NFDataX)

data MachCode
  = MachCode
  { inputX  :: Reg
  , inputY  :: Reg
  , result  :: Reg
  , aluCode :: Operator
  , ldReg   :: Reg
  , rdAddr  :: MemAddr
  , wrAddrM :: Maybe MemAddr
  , jmpM    :: Maybe Value
  }

nullCode =
  MachCode
    { inputX = Zero
    , inputY = Zero
    , result = Zero
    , aluCode = Imm
    , ldReg = Zero
    , rdAddr = 0
    , wrAddrM = Nothing
    , jmpM = Nothing
    }

data PipeRegCmd = Input | Stall | Kill

pipeReg ::
  HiddenClockResetEnable dom =>
  Trans ->
  Signal dom PipeRegCmd ->
  Signal dom Trans ->
  Signal dom Trans
pipeReg start cmd inp = medvedev go start (bundle (cmd,inp))
 where
  go s (cmdE,inpE) = case cmdE of
    Input -> inpE
    Stall -> s
    Kill  -> nop

data Cell = Cell Reg (Maybe Value)
  deriving (Generic, NFDataX)

data Trans = Trans Cell Instruction Cell Cell
  deriving (Generic, NFDataX)

dummy = Cell Zero Nothing
nop = Trans dummy Nop dummy dummy

isBranch (Trans _ (Branch _ _) _ _) = True
isBranch _ = False

isLoad (Trans _ (Load _ _) _ _) = True
isLoad _ = False

isExecTrans (Trans _ Nop _ _) = False
isExecTrans (Trans (Cell PC _) _ _ _) = False
isExecTrans _ = True

modifiesPC (Trans (Cell PC _) _ _ _) = True
modifiesPC _ = False

bypassDst :: Trans -> Trans -> Trans
bypassDst tran (Trans dst _ _ _) = updDst tran dst

updDst :: Trans -> Cell -> Trans
updDst = repDst cellHazard

repDst :: (Cell -> Cell -> Bool) -> Trans -> Cell -> Trans
repDst repFunc (Trans d o s i) cell = Trans (repCell repFunc d cell) o s i

getDstVal :: Trans -> Maybe Value
getDstVal (Trans (Cell _ v) _ _ _) = v

updSrc :: Trans -> Cell -> Cell -> Trans
updSrc (Trans dst instr src1 src2) val1 val2 =
  Trans dst instr (updCell src1 val1) (updCell src2 val2)

cellHazard :: Cell -> Cell -> Bool
cellHazard (Cell precReg pRegVal) (Cell followReg fRegVal)
  | precReg == Zero      = False
  | precReg == followReg = pRegVal /= Nothing && fRegVal /= Nothing
  | otherwise            = False

rawHazard :: Trans -> Trans -> Bool
rawHazard (Trans dst _ _ _) (Trans _ _ arg1 arg2)
  = cellHazard dst arg1 || cellHazard dst arg2

updCell :: Cell -> Cell -> Cell
updCell = repCell cellHazard

repCell :: (Cell -> Cell -> Bool) -> Cell -> Cell -> Cell
repCell replFunc bypassed replacement
  = if replFunc bypassed replacement
       then bypassed
       else replacement

bypass :: Signal dom Trans -> Signal dom Trans -> Signal dom Trans
bypass = liftA2 (\tran (Trans dst _ _ _) -> updSrc tran dst dst)

bypassMany = foldr (flip bypass)

tran `evalTrans` (dst,val) = repDst sameLoc tran (putval dst val)

sameLoc (Cell r1 _) (Cell r2 _) = r1 == r2

putval reg@(Cell r _) v = case r of
  Zero -> reg
  _ -> Cell r v

bypassRF ::
  HiddenClockResetEnable dom =>
  Signal dom Trans ->
  Signal dom Trans ->
  Signal dom Trans
bypassRF a b = mealy go (replicate d7 0) (bundle (a,b))
 where
  go regs (instr@(Trans _ _ (Cell r1 v1I) (Cell r2 v2I)),Trans (Cell rWB vWB) _ _ _) = (regs',updSrc instr val1 val2)
   where
    val1 = Cell r1 (if r1 == PC then v1I else (Just v1))
    val2 = Cell r2 (if r2 == PC then v2I else (Just v2))

    regs' = replace Zero 0
          $ maybe id (\v -> replace rWB v) vWB
          $ regs

    v1 = regs' !! r1
    v2 = regs' !! r2

exec t@(Trans dst op src1 src2) = case op of
  Nop -> t
  _ -> undefined


mem memOut trans = (rdAddr, dout, trans')
  where
    trans' = if isLoadInstr then trans `evalTrans` (loadReg,Just memOut) else trans
    (isLoadInstr,loadReg,rdAddr,dout) = case trans of
      Trans dst (Load addr _) _ _ -> (True,dst,addr,Nothing)
      Trans dst (Store r addr) _ _ -> (False,undefined,0,Just (addr,undefined))
      _ -> (False,undefined,0,Nothing)


pcTrans addr = Trans (Cell PC (Just addr)) Nop dummy dummy

cpu3 ::
  HiddenClockResetEnable dom =>
  (Signal dom Value, Signal dom Trans) ->
  (Signal dom MemAddr, Signal dom (Maybe (MemAddr,Value)), Signal dom Value)
cpu3 (memOut,instr) = (rdAddr,dout,ipntr)
 where
  --- Instruction fetch ---
  pc = pipeReg (pcTrans 0) pcLogic npc
  ipntr = fmap (fromMaybe 0 . getDstVal) pc
  pc' = pcTrans <$> ipntr + 1
  npc = mux (isBranch <$> executed) executed pc'
  pcLogic = fmap (\c -> if c then Stall else Input) dataHazard

  instr' = pipeReg nop ifLogic instr
  ifLogic = fmap (\c -> if c then Stall else Input) dataHazard

  regFetch = bypassRF instr' writeback'
  regFetch' = pipeReg nop rfLogic regFetch
  rfLogic = fmap (\c -> if c then Kill else Input) dataHazard

  executed = exec <$> (regFetch' `bypassMany` (executed' :> writeback' :> Nil))
  executed' = pipeReg nop (pure Input) executed

  (rdAddr,dout,writeback) = unbundle (mem <$> memOut <*> (executed' `bypass` writeback'))
  writeback' = pipeReg nop (pure Input) writeback

  dataHazard = execLoadHazard .||. branchExecHazard

  -- An exec-load hazard exists if a load instruction into a register Rx is
  -- immediately followed by an EX-phae instruction that uses register Rx
  execLoadHazard = isLoad <$> regFetch' .&&.
                   isExecTrans <$> regFetch .&&.
                   rawHazard <$> regFetch' <*> regFetch

  -- A branch-exec hazard exists if an EX-phase instruction is immediately followed
  -- by a branch instruction that uses the result
  branchExecHazard = isExecTrans <$> regFetch' .&&.
                     modifiesPC <$> regFetch .&&.
                     rawHazard <$> regFetch' <*> regFetch


cpu2
  :: (Vec 7 Value,Reg)    -- ^ (Register bank, Load reg addr)
  -> (Value,Instruction)  -- ^ (Memory output, Current instruction)
  -> ( (Vec 7 Value, Reg)
     , (MemAddr, Maybe (MemAddr,Value), InstrAddr)
     )
cpu2 (regbank, ldRegD) (memOut, instr) =
  ((regbank', ldRegD'), (rdAddr, (,aluOut) <$> wrAddrM, bitCoerce ipntr))
 where
  -- Current instruction pointer
  ipntr = regbank !! PC

  -- Decoder
  MachCode {..} = case instr of
    Compute op rx ry res -> nullCode {inputX=rx,inputY=ry,result=res,aluCode=op}
    Branch cr a          -> nullCode {inputX=cr,jmpM=Just a}
    Jump a               -> nullCode {aluCode=Incr,jmpM=Just a}
    Load a r             -> nullCode {ldReg=r,rdAddr=a}
    Store r a            -> nullCode {inputX=r,wrAddrM=Just a}
    Nop                  -> nullCode

  -- ALU
  regX   = regbank !! inputX
  regY   = regbank !! inputY
  aluOut = alu aluCode regX regY

  -- next instruction
  nextPC =
    case jmpM of
      Just a | aluOut /= 0 -> ipntr + a
      _                    -> ipntr + 1

  -- update registers
  ldRegD'  = ldReg  -- Delay the ldReg by 1 cycle
  regbank' = replace Zero   0
           $ replace PC     nextPC
           $ replace result aluOut
           $ replace ldRegD memOut
           $ regbank

alu Add   x y = x + y
alu Sub   x y = x - y
alu Incr  x _ = x + 1
alu Imm   x _ = x
alu CmpGt x y = if x > y then 1 else 0

system3
  :: (KnownNat n
     , HiddenClockResetEnable dom  )
  => Vec n Instruction
  -> Signal dom Value
system3 instrs = memOut
 where
  memOut = blockRam (replicate d32 0) rdAddr dout
  (rdAddr,dout,ipntr) = mealyB cpu2 ((replicate d7 0),Zero) (memOut,instr)
  instr  = asyncRom instrs <$> ipntr

prog2 = -- 0 := 4
       Compute Incr Zero RegA RegA :>
       replicate d3 (Compute Incr RegA Zero RegA) ++
       Store RegA 0 :>
       -- 1 := 6
       Compute Incr Zero RegA RegA :>
       replicate d5 (Compute Incr RegA Zero RegA) ++
       Store RegA 1 :>
       -- A := 4
       Load 0 RegA :>
       -- B := 6
       Load 1 RegB :>
       Nop :> -- Extra NOP
       -- start
       Compute CmpGt RegA RegB RegC :>
       Branch RegC 4 :>
       Compute CmpGt RegB RegA RegC :>
       Branch RegC 4 :>
       Jump 5 :>
       -- (a > b)
       Compute Sub RegA RegB RegA :>
       Jump (-6) :>
       -- (b > a)
       Compute Sub RegB RegA RegB :>
       Jump (-8) :>
       -- end
       Store RegA 2 :>
       Load 2 RegC :>
       Nil
