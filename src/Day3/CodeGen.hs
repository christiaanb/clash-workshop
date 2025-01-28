module Day3.CodeGen where

import Clash.Prelude hiding (Const (..))
import qualified Data.List as L
import Control.Monad.State

import Day3.CoreTypes

-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr0 :: Expr
expr0 = BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))

codeGen :: Expr -> [Instr]
codeGen e = []
