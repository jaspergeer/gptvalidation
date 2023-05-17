module PassManager where

import qualified UnambiguousAST as U
import qualified SymbolicExecution as EX
import qualified SBVConvert as CV
import Data.SBV (SBool, Symbolic, sAnd)

sbvOfFunction :: U.Function -> Symbolic SBool
sbvOfFunction f =
  let 
    results = EX.execute f
  in sAnd <$> mapM CV.convertExecutionResult results
