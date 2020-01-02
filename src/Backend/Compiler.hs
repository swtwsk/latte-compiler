module Backend.Compiler (compile) where

import Frontend.AST
import qualified Backend.ASTConstOptimizer as ConstOpt

import qualified Backend.QuadrupleGenerator as QuadGen

compile :: Program -> String
compile p = unlines $ show <$> (QuadGen.generate . optimize $ p)

optimize :: Program -> Program
optimize = ConstOpt.optimize
