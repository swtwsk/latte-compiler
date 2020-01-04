module Backend.X86.AsmGenerator (compile) where

import Backend.Quadruples
import Backend.FuncDef

compile :: [FuncDef] -> [String]
compile = (show <$>)


