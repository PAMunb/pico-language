module Pico.StaticAnalyser (
    analyseProgram
    ) where

import System.IO
import Pico.AbsPico

type Analysis = [String]

analyseProgram :: Program -> Analysis
analyseProgram (Program st stmt) =
  let 
    outputChanges = or (map outputUnchangedAnalysis stmt)
  in 
    case outputChanges of
      True -> []
      False -> ["Output unchanged"]

-- | Analyse if the program contains any assignment to the output variable
outputUnchangedAnalysis :: Stmt -> Bool
outputUnchangedAnalysis (Block stmt) =
  or (map outputUnchangedAnalysis stmt)

outputUnchangedAnalysis (IfThenElse _ stmtThen stmtElse) =
  or [outputUnchangedAnalysis stmtThen, outputUnchangedAnalysis stmtElse]

outputUnchangedAnalysis (IfThen _ stmtThen) =
  outputUnchangedAnalysis stmtThen

outputUnchangedAnalysis (While _ stmt) =
  outputUnchangedAnalysis stmt

outputUnchangedAnalysis (Assignment (Ident "output") _) = 
  True
outputUnchangedAnalysis _ = 
  False
