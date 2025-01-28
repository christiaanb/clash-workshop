module Day3.Core where

import Clash.Prelude hiding (Const (..))
import qualified Data.List as L

import Day3.CoreTypes
import Day3.CodeGen

-- ========================================================================
-- Processor functions

(<~) :: (KnownNat n, Enum i) => Vec n a -> (i, a) -> Vec n a
xs <~ (i,a) = replace i a xs -- Put value a on position i in list xs

alu :: Num a => Op -> a -> a -> a
alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)

core :: KnownNat n => Vec n Instr -> (Int,Int,Stack) -> (Int,Int,Stack)
core instrs (pc,sp,stack) = case instrs!!pc of

        Push n   -> (pc', sp+1 , stack <~ (sp,n))

        Calc op  -> (pc', sp-1 , stack <~ (sp-2,v))
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        EndProg  -> (-1, sp, stack)
  where
    pc' = pc+1

-- The program that results in the value of the expression (1105):
prog0 :: Vec 14 Instr
prog0 =   Push 2
       :> Push 10
       :> Calc Mul
       :> Push 3
       :> Push 4
       :> Push 11
       :> Calc Add
       :> Calc Mul
       :> Calc Add
       :> Push 12
       :> Push 5
       :> Calc Add
       :> Calc Mul
       :> EndProg
       :> Nil

-- top entity
emptyStack :: Stack
emptyStack = repeat 0

topEntity
  :: SystemClockResetEnable
  => Signal System (Int, Int, Stack)
topEntity = s
  where
    s' = core prog0 <$> s
    s  = register (0,0,emptyStack) s'

-- Testing
test :: IO ()
test = putStr
     . unlines
     . L.map show
     . takeWhile (\(pc,_,_) -> pc /= -1)
     $ sample topEntity
