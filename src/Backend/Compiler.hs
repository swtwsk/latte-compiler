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
    then (show <$>)
    else Asm.compile . FuncDef.toFuncDefs

modifyAST :: Program -> Program
modifyAST = Renamer.renameNestedVariables . ConstOpt.optimize
