module PassManager where

import qualified NormalizedAST as N
import qualified SymbolicExecution as EX
import qualified SBVConvert as CV
import Data.SBV (SBool, Symbolic, sAnd)

sbvOfNormalized :: N.Function -> Symbolic SBool
sbvOfNormalized f =
  let 
    results = EX.execute f
  in sAnd <$> mapM CV.convertExecutionResult results
