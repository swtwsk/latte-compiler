module Frontend.AST (
    Program(..),
    TopDef(..),
    Arg(..),
    Block(..),
    Stmt(..),
    Item(..),
    Type(..),
    Expr(..),
    AddOp(..),
    MulOp(..),
    RelOp(..)
) where

import Utils.StringUtils

data Program = Program [TopDef]
    deriving (Eq, Ord, Read)

data TopDef = FnDef Type String [Arg] Block
    deriving (Eq, Ord, Read)

data Arg = Arg Type String
    deriving (Eq, Ord, Read)

data Block = Block [Stmt]
    deriving (Eq, Ord, Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass String Expr
    | Incr String
    | Decr String
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
    deriving (Eq, Ord, Read)

data Item = NoInit String | Init String Expr
    deriving (Eq, Ord, Read)

data Type = TInt | TStr | TBool | TVoid | TFun Type [Type]
    deriving (Eq, Ord, Read)

data Expr
    = EVar String
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp String [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    deriving (Eq, Ord, Read)

data AddOp = Plus | Minus
    deriving (Eq, Ord, Read)

data MulOp = Times | Div | Mod
    deriving (Eq, Ord, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
    deriving (Eq, Ord, Read)

-- SHOWS
instance Show Program where
    show (Program topdefs) = unlines $ show <$> topdefs

instance Show TopDef where
    show (FnDef t i args block) =
        show t ++ " " ++ i ++ "(" ++ safeShowList args ++ ") " ++ 
        show block

instance Show Arg where
    show (Arg t i) = show t ++ " " ++ i

instance Show Block where
    show (Block stmts) = 
        "{\n" ++ unlines (indent . show <$> stmts) ++ "}\n"

instance Show Stmt where
    show Empty = ";"
    show (BStmt block) = show block
    show (Decl t items) = show t ++ " " ++ safeShowList items ++ ";"
    show (Ass i expr) = i ++ " = " ++ show expr ++ ";"
    show (Incr i) = i ++ "++;"
    show (Decr i) = i ++ "--;"
    show (Ret expr) = "return " ++ show expr ++ ";"
    show VRet = "return;"
    show (Cond expr stmt) = 
        "if (" ++ show expr ++ ")\n" ++ (indent . show $ stmt)
    show (CondElse expr ifStmt elseStmt) =
        "if (" ++ show expr ++ ")\n" ++ 
        (indent . show $ ifStmt) ++ "\nelse\n" ++
        (indent . show $ elseStmt)
    show (While expr stmt) =
        "while (" ++ show expr ++ ")\n" ++
        (indent . show $ stmt)
    show (SExp expr) = show expr ++ ";"

instance Show Item where
    show (NoInit i) = i
    show (Init i expr) = i ++ " = " ++ show expr

instance Show Type where
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
    show TVoid = "void"
    show (TFun t tl) = show t ++ "(" ++ safeShowList tl ++ ")"

instance Show Expr where
    show (EVar i) = i
    show (ELitInt i) = show i
    show ELitTrue = "true"
    show ELitFalse = "false"
    show (EApp i expr) = i ++ "(" ++ safeShowList expr ++ ")"
    show (EString s) = s
    show (Neg expr) = "-(" ++ show expr ++ ")"
    show (Not expr) = "!(" ++ show expr ++ ")"
    show (EMul e1 op e2) = unwords [show e1, show op, show e2]
    show (EAdd e1 op e2) = unwords [show e1, show op, show e2]
    show (ERel e1 op e2) = unwords [show e1, show op, show e2]
    show (EAnd e1 e2) = unwords [show e1, "&&", show e2]
    show (EOr e1 e2) = unwords [show e1, "||", show e2]

instance Show AddOp where
    show Plus  = "+"
    show Minus = "-"

instance Show MulOp where
    show Times = "*"
    show Div   = "/"
    show Mod   = "%"

instance Show RelOp where
    show LTH = "<"
    show GTH = ">"
    show EQU = "=="
    show LE  = "<="
    show GE  = ">="
    show NE  = "!="
