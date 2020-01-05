module Backend.Quadruples (
    Quadruple(..),
    Var(..),
    OpBin(..),
    OpUn(..),
    OpAdd(..),
    OpMul(..),
    OpRel(..),
    OpLog(..),
    varType
) where

import Frontend.AST (Type(..), Arg(..))
import Utils.StringUtils (safeShowList)

data Var = Var String Type
         | Temp String Type
         | CInt Integer 
         | CBool Bool 
         | CString String

type Label   = String
type FunName = String

data Quadruple = Binary Var Var OpBin Var
               | Unary Var OpUn Var
               | FunHead Type FunName [Arg]
               | Label Label
               | Assign Var Var
               | Goto Label
               | IfJmp Var Label Label -- if t then L1 else L2
               | Call FunName Int  -- call f with n arguments
               | FCall Var FunName Int
               | Param Var
               | Return (Maybe Var)
            
data OpBin = BAdd OpAdd | BMul OpMul | BRel OpRel | BLog OpLog
data OpUn  = UMinus | UNot
data OpAdd = BPlus | BMinus
data OpMul = BTimes | BDiv | BMod
data OpRel = BLTH | BLE | BGTH | BGE | BEQU | BNE
data OpLog = BAnd | BOr

varType :: Var -> Type
varType var = case var of
    Var _ t   -> t
    Temp _ t  -> t
    CInt _    -> TInt
    CBool _   -> TBool
    CString _ -> TStr

instance Show Quadruple where
    show (Binary lvar a op b) = 
        show lvar ++ " := " ++ show a ++ " " ++ show op ++ " " ++ show b
    show (Unary lvar op b) = show lvar ++ " := " ++ show op ++ " " ++ show b
    show (FunHead t fname list) = "define " ++ show t ++
        " " ++ fname ++ "(" ++ safeShowList list ++ "):"
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
    show (Var s _)   = s
    show (Temp s _)  = "%_" ++ s
    show (CInt i)    = show i
    show (CBool b)   = show b
    show (CString s) = s

instance Show OpBin where
    show op = case op of
        BAdd o -> show o
        BMul o -> show o
        BRel  o -> show o
        BLog o -> show o

instance Show OpAdd where
    show BPlus  = "+"
    show BMinus = "-"

instance Show OpMul where
    show op = case op of
        BTimes -> "*"
        BDiv -> "/"
        BMod -> "%"

instance Show OpLog where
    show BAnd = "and"
    show BOr  = "or"

instance Show OpRel where
    show op = case op of
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
