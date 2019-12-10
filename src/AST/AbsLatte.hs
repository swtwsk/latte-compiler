module AST.AbsLatte where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)

data Program a = Program a [TopDef a]
    deriving (Ord, Show, Read)

data TopDef a = FnDef a (Type a) Ident [Arg a] (Block a)
    deriving (Ord, Show, Read)

data Arg a = Arg a (Type a) Ident
    deriving (Ord, Show, Read)

data Block a = Block a [Stmt a]
    deriving (Ord, Show, Read)

data Stmt a
    = Empty a
    | BStmt a (Block a)
    | Decl a (Type a) [Item a]
    | Ass a Ident (Expr a)
    | Incr a Ident
    | Decr a Ident
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | SExp a (Expr a)
    deriving (Ord, Show, Read)

data Item a = NoInit a Ident | Init a Ident (Expr a)
    deriving (Ord, Show, Read)

data Type a
    = Int a | Str a | Bool a | Void a | Fun a (Type a) [Type a]
    deriving (Ord, Read)

data Expr a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr a]
    | EString a String
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
        (Fun _ tl t2l, Fun _ tr t2r) -> tl == tr && t2l == t2r
        _ -> False

instance Eq (Expr a) where
    l == r = case (l, r) of
        (EVar _ il, EVar _ ir) -> il == ir
        (ELitInt _ il, ELitInt _ ir) -> il == ir
        (ELitTrue _, ELitTrue _) -> True
        (ELitFalse _, ELitFalse _) -> True
        (EApp _ il el, EApp _ ir er) -> il == ir && el == er
        (EString _ sl, EString _ sr) -> sl == sr
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
        FnDef a type_ ident args block -> 
            FnDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)

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
        Ass a ident expr -> Ass (f a) ident (fmap f expr)
        Incr a ident -> Incr (f a) ident
        Decr a ident -> Decr (f a) ident
        Ret a expr -> Ret (f a) (fmap f expr)
        VRet a -> VRet (f a)
        Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
        CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
        While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
        SExp a expr -> SExp (f a) (fmap f expr)

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
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)

instance Functor Expr where
    fmap f x = case x of
        EVar a ident -> EVar (f a) ident
        ELitInt a integer -> ELitInt (f a) integer
        ELitTrue a -> ELitTrue (f a)
        ELitFalse a -> ELitFalse (f a)
        EApp a ident exprs -> EApp (f a) ident (map (fmap f) exprs)
        EString a string -> EString (f a) string
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
    show (Fun _ t tl) = show t ++ "(" ++ safeShowList tl ++ ")"

instance Show (Expr a) where
    show (EVar _ (Ident i)) = i
    show (ELitInt _ i) = show i
    show (ELitTrue _) = "true"
    show (ELitFalse _) = "false"
    show (EApp _ (Ident i) expr) = i ++ "(" ++ safeShowList expr ++ ")"
    show (EString _ s) = s
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
