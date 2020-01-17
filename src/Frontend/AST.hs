module Frontend.AST (
    Program(..),
    TopDef(..),
    ClassDecl(..),
    FnDef(..),
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

data TopDef = FnTopDef FnDef
            | ClassDef String [ClassDecl]
            | ClassExtDef String String [ClassDecl]
            deriving (Eq, Ord, Read)

data ClassDecl = MethodDef FnDef | FieldDef Type String
               deriving (Eq, Ord, Read)

data FnDef = FnDef Type String [Arg] Block
    deriving (Eq, Ord, Read)

data Arg = Arg Type String
    deriving (Eq, Ord, Read)

data Block = Block [Stmt]
    deriving (Eq, Ord, Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass Expr Expr
    | Incr Expr
    | Decr Expr
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
    | For Type String Expr Stmt
    deriving (Eq, Ord, Read)

data Item = NoInit String | Init String Expr
    deriving (Eq, Ord, Read)

data Type = TInt 
          | TStr 
          | TBool 
          | TVoid 
          | TArray Type 
          | TClass String
          | TFun Type [Type]
          | TPointer Type
    deriving (Eq, Ord, Read)

data Expr
    = EVar String
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp String [Expr]
    | EString String
    | EArr Type Expr
    | EClass Type
    | EArrGet Expr Expr
    | EFieldGet Expr String
    | EMethod Expr String [Expr]
    | ENull String
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
    show (FnTopDef fndef) = show fndef
    show (ClassDef name decl) = "class " ++ name ++ " {\n" ++
        unlines (indent . show <$> decl) ++ "}\n"
    show (ClassExtDef name extends decl) = 
        "class " ++ name ++ " extends " ++ extends ++ " {\n" ++
        unlines (indent . show <$> decl) ++ "}\n"

instance Show ClassDecl where
    show (MethodDef fndef) = show fndef
    show (FieldDef t i) = show t ++ " " ++ i ++ ";"

instance Show FnDef where
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
    show (Ass e expr) = show e ++ " = " ++ show expr ++ ";"
    show (Incr e) = show e ++ "++;"
    show (Decr e) = show e ++ "--;"
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
    show (For t name expr stmt) = "for (" ++ show t ++ " " ++ name ++ 
        " : " ++ show expr ++ ")\n" ++ show stmt 

instance Show Item where
    show (NoInit i) = i
    show (Init i expr) = i ++ " = " ++ show expr

instance Show Type where
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
    show TVoid = "void"
    show (TArray t) = show t ++ "[]"
    show (TClass name) = name
    show (TFun t tl) = show t ++ "(" ++ safeShowList tl ++ ")"
    show (TPointer t) = show t ++ "*"

instance Show Expr where
    show (EVar i) = i
    show (ELitInt i) = show i
    show ELitTrue = "true"
    show ELitFalse = "false"
    show (EApp i expr) = i ++ "(" ++ safeShowList expr ++ ")"
    show (EString s) = s
    show (EArr t expr) = "new " ++ show t ++ "[" ++ show expr ++ "]"
    show (EClass t) = "new " ++ show t
    show (EArrGet e1 e2) = show e1 ++ "[" ++ show e2 ++ "]"
    show (EFieldGet e1 i) = show e1 ++ "." ++ i
    show (EMethod e1 name el) = show e1 ++ "." ++ name ++ "(" ++
        safeShowList el ++ ")"
    show (ENull i) = "(" ++ i ++ ") null"
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
