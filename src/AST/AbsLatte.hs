module AST.AbsLatte where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)

data Program a = Program a [TopDef a]
  deriving (Ord, Show, Read)

data TopDef a
    = FnTopDef a (FnDef a)
    | ClassExtDef a Ident Ident [ClassDecl a]  -- for the ord instance
    | ClassDef a Ident [ClassDecl a]
  deriving (Ord, Show, Read)

data ClassDecl a
    = MethodDef a (FnDef a) | FieldDef a (Type a) Ident
  deriving (Ord, Show, Read)

data FnDef a = FnDef a (Type a) Ident [Arg a] (Block a)
  deriving (Ord, Show, Read)

data Arg a = Arg a (Type a) Ident
  deriving (Ord, Show, Read)

data Block a = Block a [Stmt a]
  deriving (Ord, Show, Read)

data Stmt a
    = Empty a
    | BStmt a (Block a)
    | Decl a (Type a) [Item a]
    | Ass a (Expr a) (Expr a)
    | Incr a (Expr a)
    | Decr a (Expr a)
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | SExp a (Expr a)
    | For a (Type a) Ident (Expr a) (Stmt a)
  deriving (Ord, Show, Read)

data Item a = NoInit a Ident | Init a Ident (Expr a)
  deriving (Ord, Show, Read)

data Type a
    = Int a
    | Str a
    | Bool a
    | Void a
    | Array a (Type a)
    | Class a Ident
    | Fun a (Type a) [Type a]
    | Pointer a (Type a)
  deriving (Ord, Read)

data Expr a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr a]
    | EString a String
    | EArr a (Type a) (Expr a)
    | EClass a (Type a)
    | EArrGet a (Expr a) (Expr a)
    | EFieldGet a (Expr a) Ident
    | EMethod a (Expr a) Ident [Expr a]
    | ENull a Ident
    | Neg a (Expr a)
    | Not a (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Ord, Read)

data AddOp a = Plus a | Minus a
  deriving (Ord, Read)

data MulOp a = Times a | Div a | Mod a
  deriving (Ord, Read)

data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Ord, Read)

-- EQ
instance Eq (Program a) where
    (Program _ ll) == (Program _ lr) = ll == lr

instance Eq (TopDef a) where
    (FnTopDef _ fl) == (FnTopDef _ fr) = fl == fr
    (ClassDef _ il cl) == (ClassDef _ ir cr) = il == ir && cl == cr
    (ClassExtDef _ il extl cl) == (ClassExtDef _ ir extr cr) =
        il == ir && extl == extr && cl == cr

instance Eq (ClassDecl a) where
    (MethodDef _ fl) == (MethodDef _ fr) = fl == fr
    (FieldDef _ tl il) == (FieldDef _ tr ir) = tl == tr && il == ir

instance Eq (FnDef a) where
    (FnDef _ tl il argsl blockl) == (FnDef _ tr ir argsr blockr) =
        tl == tr && il == ir && argsl == argsr && blockl == blockr

instance Eq (Arg a) where
    (Arg _ tl il) == (Arg _ tr ir) = tl == tr && il == ir

instance Eq (Block a) where
    (Block _ stmtsl) == (Block _ stmtsr) = stmtsl == stmtsr

instance Eq (Stmt a) where
    l == r = case (l, r) of
        (Empty _, Empty _) -> True
        (BStmt _ bl, BStmt _ br) -> bl == br
        (Decl _ tl il, Decl _ tr ir) -> tl == tr && il == ir
        (Ass _ il el, Ass _ ir er) -> il == ir && el == er
        (Incr _ l, Incr _ r) -> l == r
        (Decr _ l, Decr _ r) -> l == r
        (Ret _ el, Ret _ er) -> el == er
        (VRet _, VRet _) -> True
        (Cond _ el sl, Cond _ er sr) -> el == er && sl == sr
        (CondElse _ el sl s2l, CondElse _ er sr s2r) ->
            el == er && sl == sr && s2l == s2r
        (While _ el sl, While _ er sr) -> el == er && sl == sr
        (SExp _ el, SExp _ er) -> el == er
        (For _ tl il el sl, For _ tr ir er sr) -> 
            tl == tr && il == ir && el == er && sl == sr
        _ -> False

instance Eq (Item a) where
    (NoInit _ il) == (NoInit _ ir) = il == ir
    (Init _ il el) == (Init _ ir er) = il == ir && el == er
    _ == _ = False

instance Eq (Type a) where
    l == r = case (l, r) of
        (Int _, Int _) -> True
        (Str _, Str _) -> True
        (Bool _, Bool _) -> True
        (Void _, Void _) -> True
        (Array _ l, Array _ r) -> l == r
        (Class _ l, Class _ r) -> l == r
        (Fun _ tl t2l, Fun _ tr t2r) -> tl == tr && t2l == t2r
        (Pointer _ l, Pointer _ r) -> l == r
        _ -> False

instance Eq (Expr a) where
    l == r = case (l, r) of
        (EVar _ il, EVar _ ir) -> il == ir
        (ELitInt _ il, ELitInt _ ir) -> il == ir
        (ELitTrue _, ELitTrue _) -> True
        (ELitFalse _, ELitFalse _) -> True
        (EApp _ il el, EApp _ ir er) -> il == ir && el == er
        (EString _ sl, EString _ sr) -> sl == sr
        (EArr _ tl el, EArr _ tr er) -> tl == tr && el == er
        (EClass _ tl, EClass _ tr) -> tl == tr
        (EArrGet _ e1l e2l, EArrGet _ e1r e2r) -> e1l == e1r && e2l == e2r
        (EFieldGet _ el il, EFieldGet _ er ir) -> el == er && il == ir
        (EMethod _ el il exls, EMethod _ er ir exrs) -> 
            el == er && il == ir && exls == exrs
        (ENull _ il, ENull _ ir) -> il == ir
        (Neg _ el, Neg _ er) -> el == er
        (Not _ el, Not _ er) -> el == er
        (EMul _ el opl e2l, EMul _ er opr e2r) -> 
            el == er && opl == opr && e2l == e2r
        (EAdd _ el opl e2l, EAdd _ er opr e2r) -> 
            el == er && opl == opr && e2l == e2r
        (ERel _ el opl e2l, ERel _ er opr e2r) -> 
            el == er && opl == opr && e2l == e2r
        (EAnd _ el e2l, EAnd _ er e2r) -> el == er && e2l == e2r
        (EOr _ el e2l, EOr _ er e2r) -> el == er && e2l == e2r

instance Eq (AddOp a) where
    Plus _  == Plus _  = True
    Minus _ == Minus _ = True
    _ == _ = False

instance Eq (MulOp a) where
    Times _ == Times _ = True
    Div _   == Div _   = True
    Mod _   == Mod _   = True
    _ == _ = False

instance Eq (RelOp a) where
    LTH _ == LTH _ = True
    GTH _ == GTH _ = True
    EQU _ == EQU _ = True
    LE _  == LE _  = True
    GE _  == GE _  = True
    NE _  == NE _  = True

-- FUNCTOR
instance Functor Program where
    fmap f x = case x of
        Program a topdefs -> Program (f a) (map (fmap f) topdefs)

instance Functor TopDef where
    fmap f x = case x of
        FnTopDef a fndef -> FnTopDef (f a) (fmap f fndef)
        ClassDef a ident classdecls -> ClassDef (f a) ident (map (fmap f) classdecls)
        ClassExtDef a ident1 ident2 classdecls -> ClassExtDef (f a) ident1 ident2 (map (fmap f) classdecls)

instance Functor ClassDecl where
    fmap f x = case x of
        MethodDef a fndef -> MethodDef (f a) (fmap f fndef)
        FieldDef a type_ ident -> FieldDef (f a) (fmap f type_) ident

instance Functor FnDef where
    fmap f x = case x of
        FnDef a type_ ident args block -> FnDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)

instance Functor Arg where
    fmap f x = case x of
        Arg a type_ ident -> Arg (f a) (fmap f type_) ident

instance Functor Block where
    fmap f x = case x of
        Block a stmts -> Block (f a) (map (fmap f) stmts)

instance Functor Stmt where
    fmap f x = case x of
        Empty a -> Empty (f a)
        BStmt a block -> BStmt (f a) (fmap f block)
        Decl a type_ items -> Decl (f a) (fmap f type_) (map (fmap f) items)
        Ass a expr1 expr2 -> Ass (f a) (fmap f expr1) (fmap f expr2)
        Incr a expr -> Incr (f a) (fmap f expr)
        Decr a expr -> Decr (f a) (fmap f expr)
        Ret a expr -> Ret (f a) (fmap f expr)
        VRet a -> VRet (f a)
        Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
        CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
        While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
        SExp a expr -> SExp (f a) (fmap f expr)
        For a type_ ident expr stmt -> For (f a) (fmap f type_) ident (fmap f expr) (fmap f stmt)

instance Functor Item where
    fmap f x = case x of
        NoInit a ident -> NoInit (f a) ident
        Init a ident expr -> Init (f a) ident (fmap f expr)

instance Functor Type where
    fmap f x = case x of
        Int a -> Int (f a)
        Str a -> Str (f a)
        Bool a -> Bool (f a)
        Void a -> Void (f a)
        Array a type_ -> Array (f a) (fmap f type_)
        Class a ident -> Class (f a) ident
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)
        Pointer a type_ -> Pointer (f a) (fmap f type_)

instance Functor Expr where
    fmap f x = case x of
        EVar a ident -> EVar (f a) ident
        ELitInt a integer -> ELitInt (f a) integer
        ELitTrue a -> ELitTrue (f a)
        ELitFalse a -> ELitFalse (f a)
        EApp a ident exprs -> EApp (f a) ident (map (fmap f) exprs)
        EString a string -> EString (f a) string
        EArr a type_ expr -> EArr (f a) (fmap f type_) (fmap f expr)
        EClass a type_ -> EClass (f a) (fmap f type_)
        EArrGet a expr1 expr2 -> EArrGet (f a) (fmap f expr1) (fmap f expr2)
        EFieldGet a expr ident -> EFieldGet (f a) (fmap f expr) ident
        EMethod a expr ident exprs -> EMethod (f a) (fmap f expr) ident (map (fmap f) exprs)
        ENull a ident -> ENull (f a) ident
        Neg a expr -> Neg (f a) (fmap f expr)
        Not a expr -> Not (f a) (fmap f expr)
        EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
        EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)

instance Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        Minus a -> Minus (f a)

instance Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a -> Div (f a)
        Mod a -> Mod (f a)

instance Functor RelOp where
    fmap f x = case x of
        LTH a -> LTH (f a)
        LE a -> LE (f a)
        GTH a -> GTH (f a)
        GE a -> GE (f a)
        EQU a -> EQU (f a)
        NE a -> NE (f a)

-- SHOW
instance Show (Type a) where
    show (Int _) = "int"
    show (Str _) = "string"
    show (Bool _) = "bool"
    show (Void _) = "void"
    show (Array _ t) = show t ++ "[]"
    show (Class _ (Ident i)) = i
    show (Pointer _ t) = show t ++ "*"
    show (Fun _ t tl) = show t ++ "(" ++ safeShowList tl ++ ")"

instance Show (Expr a) where
    show (EVar _ (Ident i)) = i
    show (ELitInt _ i) = show i
    show (ELitTrue _) = "true"
    show (ELitFalse _) = "false"
    show (EApp _ (Ident i) expr) = i ++ "(" ++ safeShowList expr ++ ")"
    show (EString _ s) = s
    show (EArr _ t expr) = "new " ++ show t ++ "[" ++ show expr ++ "]"
    show (EClass _ t) = "new " ++ show t
    show (EArrGet _ e1 e2) = show e1 ++ "[" ++ show e2 ++ "]"
    show (EFieldGet _ e1 (Ident i)) = show e1 ++ "." ++ i
    show (EMethod _ e1 (Ident name) el) = show e1 ++ "." ++ name ++ "(" ++
        safeShowList el ++ ")"
    show (ENull _ (Ident i)) = "(" ++ i ++ ") null"
    show (Neg _ expr) = "-(" ++ show expr ++ ")"
    show (Not _ expr) = "!(" ++ show expr ++ ")"
    show (EMul _ e1 op e2) = unwords [show e1, show op, show e2]
    show (EAdd _ e1 op e2) = unwords [show e1, show op, show e2]
    show (ERel _ e1 op e2) = unwords [show e1, show op, show e2]
    show (EAnd _ e1 e2) = unwords [show e1, "&&", show e2]
    show (EOr _ e1 e2) = unwords [show e1, "||", show e2]

safeShowList :: Show a => [a] -> String
safeShowList l = case l of
    _:_ -> foldr1 (\el -> ((el ++ ", ") ++)) (fmap show l)
    _ -> ""

instance Show (AddOp a) where
    show (Plus _)  = "+"
    show (Minus _) = "-"

instance Show (MulOp a) where
    show (Times _) = "*"
    show (Div _)   = "/"
    show (Mod _)   = "%"

instance Show (RelOp a) where
    show (LTH _) = "<"
    show (GTH _) = ">"
    show (EQU _) = "=="
    show (LE _)  = "<="
    show (GE _)  = ">="
    show (NE _)  = "!="

-- Extract ---
class (Functor f) => Extract f where
    extract :: f a -> a

instance Extract Program where
    extract (Program pos _) = pos

instance Extract TopDef where
    extract (FnTopDef pos _) = pos
    extract (ClassExtDef pos _ _ _) = pos
    extract (ClassDef pos _ _) = pos

instance Extract ClassDecl where
    extract (MethodDef pos _) = pos
    extract (FieldDef pos _ _) = pos

instance Extract FnDef where
    extract (FnDef pos _ _ _ _) = pos

instance Extract Arg where
    extract (Arg pos _ _) = pos

instance Extract Block where
    extract (Block pos _) = pos

instance Extract Stmt where
    extract stmt = case stmt of
        Empty pos -> pos
        (BStmt pos _) -> pos
        (Decl pos _ _) -> pos
        (Ass pos _ _) -> pos
        (Incr pos _) -> pos
        (Decr pos _) -> pos
        (Ret pos _) -> pos
        (VRet pos) -> pos
        (Cond pos _ _) -> pos
        (CondElse pos _ _ _) -> pos 
        (While pos _ _) -> pos
        (SExp pos _) -> pos
        (For pos _ _ _ _) -> pos

instance Extract Item where
    extract (NoInit pos _) = pos
    extract (Init pos _ _) = pos

instance Extract Type where
    extract (Int pos) = pos
    extract (Str pos) = pos
    extract (Bool pos) = pos
    extract (Void pos) = pos
    extract (Array pos _) = pos
    extract (Class pos _) = pos
    extract (Fun pos _ _) = pos
    extract (Pointer pos _) = pos

instance Extract Expr where
    extract (EVar pos _) = pos
    extract (ELitInt pos _) = pos
    extract (ELitTrue pos) = pos
    extract (ELitFalse pos) = pos
    extract (EApp pos _ _) = pos
    extract (EString pos _) = pos
    extract (EArr pos _ _) = pos
    extract (EClass pos _) = pos
    extract (EArrGet pos _ _) = pos
    extract (EFieldGet pos _ _) = pos
    extract (EMethod pos _ _ _) = pos
    extract (ENull pos _) = pos
    extract (Neg pos _) = pos
    extract (Not pos _) = pos
    extract (EMul pos _ _ _) = pos
    extract (EAdd pos _ _ _) = pos
    extract (ERel pos _ _ _) = pos
    extract (EAnd pos _ _) = pos
    extract (EOr pos _ _) = pos

instance Extract AddOp where
    extract (Plus pos) = pos
    extract (Minus pos) = pos

instance Extract MulOp where
    extract (Times pos) = pos
    extract (Div pos) = pos
    extract (Mod pos) = pos

instance Extract RelOp where
    extract (LTH pos) = pos
    extract (GTH pos) = pos
    extract (EQU pos) = pos
    extract (LE pos) = pos
    extract (GE pos) = pos
    extract (NE pos) = pos
