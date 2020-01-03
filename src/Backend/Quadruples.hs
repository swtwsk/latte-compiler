module Backend.Quadruples (
    Quadruple(..),
    Var(..),
    OpBin(..),
    OpUn(..),
) where

import Frontend.AST (Type(..), Arg(..))
import Utils.StringUtils (safeShowList)

data Var = Var String 
         | Temp String 
         | CInt Integer 
         | CBool Bool 
         | CString String

type Label = String

data Quadruple = Binary Var Var OpBin Var
               | Unary Var OpUn Var
               | FunHead Type Label [Arg]
               | Label Label
               | Assign Var Var
               | Goto Label
               | IfJmp Var Label Label -- if t then L1 else L2
               | Call Label Int  -- call f with n arguments
               | FCall Var Label Int
               | Param Var
               | Return (Maybe Var)
            
data OpBin = BPlus | BMinus | BTimes | BDiv | BMod | BAnd | BOr 
           | BLTH | BLE | BGTH | BGE | BEQU | BNE
data OpUn  = UMinus | UNot

instance Show Quadruple where
    show (Binary lvar a op b) = 
        show lvar ++ " := " ++ show a ++ " " ++ show op ++ " " ++ show b
    show (Unary lvar op b) = show lvar ++ " := " ++ show op ++ " " ++ show b
    show (FunHead t fname list) = "define " ++ show t ++
        " " ++ fname ++ "(" ++ (safeShowList list) ++ "):"
    show (Label label) = showLabel label
    show (Assign lvar rvar) = show lvar ++ " := " ++ show rvar
    show (Goto glabel) = "goto " ++ glabel
    show (IfJmp var ifLabel elseLabel) = 
        "if " ++ show var ++ " goto " ++ ifLabel ++ " else " ++ elseLabel
    show (Call flabel i) = "call " ++ flabel ++ ", " ++ show i
    show (FCall lvar flabel i) = 
        show lvar ++ " := " ++ "fcall " ++ flabel ++ ", " ++ show i
    show (Param var) = "param " ++ show var
    show (Return var) = "return " ++ maybe "" show var

instance Show Var where
    show (Var s)     = s
    show (Temp s)    = "%_" ++ s
    show (CInt i)    = show i
    show (CBool b)   = show b
    show (CString s) = s

instance Show OpBin where
    show op = case op of
        BPlus -> "add"
        BMinus -> "sub"
        BTimes -> "mul"
        BDiv -> "div"
        BMod -> "mod"
        BAnd -> "and"
        BOr -> "or"
        BLTH -> "<"
        BLE  -> "<="
        BGTH -> ">"
        BGE  -> ">=" 
        BEQU -> "=="
        BNE  -> "!="

instance Show OpUn where
    show op = case op of
        UMinus -> "neg"
        UNot -> "not"

showLabel :: Label -> String
showLabel l = l ++ ": "
