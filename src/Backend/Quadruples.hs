module Backend.Quadruples (
    Quadruple(..),
    Var(..),
    OpBin(..),
    OpUn(..),
    -- OpRel(..)
) where

import Frontend.AST (Type(..), Arg(..))
import Utils.StringUtils (safeShowList)

data Var = Var String | CInt Integer | CBool Bool | CString String
type Label = String

data Quadruple = Binary (Maybe Label) Var Var OpBin Var
               | Unary (Maybe Label) Var OpUn Var
               | FunHead Type Label [Arg]
            --     ULoad (Maybe Label) Var Var
            --     UStore (Maybe Label) Var Var
               | Label Label  -- for now
               | Assign (Maybe Label) Var Var
               | Goto (Maybe Label) Label
               | IfJmp (Maybe Label) Var Label Label -- if t then L1 else L2
               | Call (Maybe Label) Label Int  -- call f with n arguments
               | FCall (Maybe Label) Var Label Int  -- TODO: różnicuj to!
               | Param (Maybe Label) Var
               | Return (Maybe Label) (Maybe Var)

-- data RetVal = RetInt Int | RetBool Bool | RetVar Var
            
data OpBin = BPlus | BMinus | BTimes | BDiv | BMod | BAnd | BOr 
           | BLTH | BLE | BGTH | BGE | BEQU | BNE
data OpUn  = UMinus | UNot  -- | ULoad | UStore
-- data OpRel = LTH | LE | GTH | GE | EQU | NE

-- data Quadruple = Binary Var Var OpBin Var
--                 Unary Var OpUn Var
--                 Copy Var Var
--                 Label Label
--                 Goto Label
--                 IfJmp Var OpRel Var Label
--                 Call Label Int  -- call f with n arguments
--                 Param Var
--                 Return (Maybe RetVal)

instance Show Quadruple where
    show (Binary label lvar a op b) = mShowLabel label ++ 
        show lvar ++ " := " ++ show a ++ " " ++ show op ++ " " ++ show b
    show (Unary label lvar op b) = mShowLabel label ++
        show lvar ++ " := " ++ show op ++ " " ++ show b
    show (FunHead t fname list) = "define " ++ show t ++
        " " ++ fname ++ "(" ++ (safeShowList list) ++ "):"
    -- show (ULoad label lvar rvar) = mShowLabel label ++
    --     show lvar ++ " := load " ++ show rvar
    -- show (UStore label vvar svar) = mShowLabel label ++
    --     "store " ++ show vvar ++ " into " ++ show svar
    show (Label label) = showLabel label
    show (Assign label lvar rvar) = mShowLabel label ++
        show lvar ++ " := " ++ show rvar
    show (Goto label glabel) = mShowLabel label ++ "goto " ++ glabel
    show (IfJmp label var ifLabel elseLabel) = mShowLabel label ++ 
        "if " ++ show var ++ " goto " ++ ifLabel ++ " else " ++ elseLabel
    show (Call label flabel i) = mShowLabel label ++ "call " ++ flabel ++ 
        ", " ++ show i
    show (FCall label lvar flabel i) = mShowLabel label ++ 
        show lvar ++ " := " ++ "fcall " ++ flabel ++ ", " ++ show i
    show (Param label var) = mShowLabel label ++ "param " ++ show var
    show (Return label var) = mShowLabel label ++ 
        "return " ++ maybe "" show var

instance Show Var where
    show (Var s) = s
    show (CInt i) = show i
    show (CBool b) = show b
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

-- instance Show OpRel where
--     show op = case op of
--         LTH -> "<"
--         LE  -> "<="
--         GTH -> ">"
--         GE  -> ">=" 
--         EQU -> "=="
--         NE  -> "!="

mShowLabel :: Maybe Label -> String
mShowLabel = maybe "" showLabel

showLabel :: Label -> String
showLabel l = l ++ ": "
