module Backend.Compiler (compile) where

import Frontend.AST
import qualified Backend.ASTConstOptimizer as ConstOpt
import qualified Backend.ASTVariableRenamer as Renamer
import qualified Backend.Quadruples as Quads
import qualified Backend.QuadrupleGenerator as QuadGen
import qualified Backend.X86.AsmGenerator as Asm
import qualified Backend.FuncDef as FuncDef
import Utils.StringUtils

compile :: Bool -> Program -> String
compile showQuads = unlines . compileFromQuads showQuads . 
    QuadGen.generate . modifyAST

compileFromQuads :: Bool -> [Quads.Quadruple] -> [String]
compileFromQuads showQuads = if showQuads 
    then showFun
    else Asm.compile . FuncDef.toFuncDefs
    where
        showFun (h1:(f@Quads.FunHead {}):t) =
            [showIndented h1, "", showIndented f] ++ showFun t
        showFun (h:t) = showIndented h : showFun t
        showFun [] = []

        showIndented quad = case quad of
            l@Quads.Label {}   -> show l
            f@Quads.FunHead {} -> show f
            x -> indent . show $ x

modifyAST :: Program -> Program
modifyAST = Renamer.renameNestedVariables . ConstOpt.optimize
