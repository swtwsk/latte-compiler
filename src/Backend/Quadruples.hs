module Backend.Quadruples (
    Quadruple(..),
    Var(..),
    FunName(..),
    OpBin(..),
    OpUn(..),
    OpAdd(..),
    OpMul(..),
    OpRel(..),
    OpLog(..),
    varType
) where

import qualified Data.Map as Map

import Frontend.AST (Type(..), Arg(..))
import Utils.StringUtils (safeShowList)

data Var = Var String Type
         | Temp String Type
         | CNull Type
         | CInt Integer 
         | CBool Bool 
         | CString String

type Label   = String
data FunName = FunName String | MethodName String String

data Quadruple = Binary Var Var OpBin Var
               | Unary Var OpUn Var
               | FunHead Type FunName [Arg]
               | Label Label
               | Assign Var Var
               | Goto Label
               | IfJmp Var Label Label -- if t then L1 else L2
               | WhileJmp Var Label Label  -- while v { L1 } L2
               | Call FunName Int  -- call f with n arguments
               | FCall Var FunName Int
               | Param Var
               | Return (Maybe Var)
               | ArrSize Var Var      -- x := var.length
               | ArrLoad Var Var Var  -- x := y[z]
               | ArrStore Var Var Var -- x[y] := y
               | ClassLoad Var Var Int -- x := y.field
               | ClassStore Var Int Var -- x.field := y
               | Debug String

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
    CNull t   -> t
    CInt _    -> TInt
    CBool _   -> TBool
    CString _ -> TStr

instance Show Quadruple where
    show (Binary lvar a op b) = 
        show lvar ++ " := " ++ show a ++ " " ++ show op ++ " " ++ show b
    show (Unary lvar op b) = show lvar ++ " := " ++ show op ++ " " ++ show b
    show (FunHead t fname list) = "define " ++ show t ++
        " " ++ show fname ++ "(" ++ safeShowList list ++ "):"
    show (Label label) = showLabel label
    show (Assign lvar rvar) = show lvar ++ " := " ++ show rvar
    show (Goto glabel) = "goto " ++ glabel
    show (IfJmp var ifLabel elseLabel) = 
        "if " ++ show var ++ " goto " ++ ifLabel ++ " else " ++ elseLabel
    show (WhileJmp var ifLabel elseLabel) = 
        "while " ++ show var ++ " goto " ++ ifLabel ++ " else " ++ elseLabel
    show (Call flabel i) = "call " ++ show flabel ++ ", " ++ show i
    show (FCall lvar flabel i) = 
        show lvar ++ " := " ++ "fcall " ++ show flabel ++ ", " ++ show i
    show (Param var) = "param " ++ show var
    show (Return var) = "return " ++ maybe "" show var
    show (ArrSize v arr) = show v ++ " := getsize " ++ show arr
    show (ArrLoad x y z) = show x ++ " := load " ++ show y ++ ", " ++ show z
    show (ArrStore x y z) = 
        "store " ++ show x ++ " [" ++ show y ++ "], " ++ show z
    show (ClassLoad x y field) = show x ++ " := loadfld " ++ show y ++ 
        " . " ++ show field
    show (ClassStore x field y) = "storefld " ++ show x ++ " . " ++ 
        show field ++ ", " ++ show y
    show (Debug s) = s

instance Show Var where
    show (Var s _)   = s
    show (Temp s _)  = "%" ++ s
    show (CNull t)   = "null"
    show (CInt i)    = show i
    show (CBool b)   = show b
    show (CString s) = '\"' : s ++ "\""

instance Show FunName where
    show (FunName s) = s
    show (MethodName cn mn) = "_cn_" ++ cn ++ "__" ++ mn

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
