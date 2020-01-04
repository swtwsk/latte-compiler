module Backend.Compiler (compile) where

import Frontend.AST
import qualified Backend.ASTConstOptimizer as ConstOpt
import qualified Backend.Quadruples as Quads
import qualified Backend.QuadrupleGenerator as QuadGen
import qualified Backend.X86.AsmGenerator as Asm
import qualified Backend.FuncDef as FuncDef
import Utils.StringUtils

compile :: Bool -> Program -> String
compile showQuads = unlines . 
    (if showQuads then (show <$>) else Asm.compile . FuncDef.toFuncDefs) .
    QuadGen.generate . optimize
    where
        showFun (h1:(f@Quads.FunHead {}):t) =
            [showIndented h1, "", showIndented f] ++ showFun t
        showFun (h:t) = showIndented h : showFun t
        showFun [] = []

        showIndented quad = case quad of
            l@Quads.Label {}   -> show l
            f@Quads.FunHead {} -> show f
            x -> indent . show $ x

optimize :: Program -> Program
optimize = ConstOpt.optimize
