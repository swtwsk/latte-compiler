{-# LANGUAGE MultiParamTypeClasses #-}

module Frontend.TranspileAST (
    transpile,
) where

import Frontend.AST
import qualified AST.AbsLatte as Abs

class Transpilable a b where
    transpile :: a -> b

instance Transpilable (Abs.Program a) Program where
    transpile (Abs.Program _ topdefs) = Program (transpile <$> topdefs)

instance Transpilable (Abs.TopDef a) TopDef where
    transpile (Abs.FnTopDef _ fndef) = FnTopDef (transpile fndef)
    transpile (Abs.ClassDef _ (Abs.Ident i) cldecls) = 
        ClassDef i (transpile <$> cldecls)
    transpile (Abs.ClassExtDef _ (Abs.Ident i1) (Abs.Ident i2) cldecls) =
        ClassExtDef i1 i2 (transpile <$> cldecls)

instance Transpilable (Abs.ClassDecl a) ClassDecl where
    transpile (Abs.MethodDef _ fndef) = MethodDef (transpile fndef)
    transpile (Abs.FieldDef _ t (Abs.Ident i)) = FieldDef (transpile t) i

instance Transpilable (Abs.FnDef a) FnDef where
    transpile (Abs.FnDef _ t (Abs.Ident i) _rgs block) =
        FnDef (transpile t) i (transpile <$> _rgs) (transpile block)

instance Transpilable (Abs.Arg a) Arg where
    transpile (Abs.Arg _ t (Abs.Ident i)) = Arg (transpile t) i

instance Transpilable (Abs.Block a) Block where
    transpile (Abs.Block _ stmts) = Block $ transpile <$> stmts

instance Transpilable (Abs.Stmt a) Stmt where
    transpile (Abs.Empty _) = Empty
    transpile (Abs.BStmt _ block) = BStmt (transpile block)
    transpile (Abs.Decl _ t items) = Decl (transpile t) (transpile <$> items)
    transpile (Abs.Ass _ e expr) = Ass (transpile e) (transpile expr)
    transpile (Abs.Incr _ e) = Incr (transpile e)
    transpile (Abs.Decr _ e) = Decr (transpile e)
    transpile (Abs.Ret _ expr) = Ret (transpile expr)
    transpile (Abs.VRet _) = VRet
    transpile (Abs.Cond _ expr stmt) = 
        transpileTwo Cond expr stmt
    transpile (Abs.CondElse _ expr ifStmt elseStmt) =
        transpileThree CondElse expr ifStmt elseStmt
    transpile (Abs.While _ expr stmt) = transpileTwo While expr stmt
    transpile (Abs.SExp _ expr) = SExp (transpile expr)
    transpile (Abs.For _ t (Abs.Ident i) expr stmt) = 
        For (transpile t) i (transpile expr) (transpile stmt)

instance Transpilable (Abs.Item a) Item where
    transpile (Abs.NoInit _ (Abs.Ident i)) = NoInit i
    transpile (Abs.Init _ (Abs.Ident i) expr) = Init i (transpile expr)

instance Transpilable (Abs.Type a) Type where
    transpile (Abs.Int _) = TInt
    transpile (Abs.Str _) = TStr
    transpile (Abs.Bool _) = TBool
    transpile (Abs.Void _) = TVoid
    transpile (Abs.Array _ t) = TArray (transpile t)
    transpile (Abs.Class _ (Abs.Ident i)) = TClass i
    transpile (Abs.Fun _ t types) = TFun (transpile t) (transpile <$> types)
    transpile (Abs.Pointer _ t) = TPointer (transpile t)

instance Transpilable (Abs.Expr a) Expr where
    transpile e = case e of
        Abs.EVar _ (Abs.Ident i) -> EVar i
        Abs.ELitInt _ i -> ELitInt i
        Abs.ELitTrue _ -> ELitTrue
        Abs.ELitFalse _ -> ELitFalse
        Abs.EApp _ (Abs.Ident i) exprs -> EApp i (transpile <$> exprs)
        Abs.EString _ s -> EString s
        Abs.EArr _ t e -> EArr (transpile t) (transpile e)
        Abs.EClass _ t -> EClass (transpile t)
        Abs.EArrGet _ e1 e2 -> EArrGet (transpile e1) (transpile e2)
        Abs.EFieldGet _ e (Abs.Ident i) -> EFieldGet (transpile e) i
        Abs.EMethod _ e (Abs.Ident i) exprs -> 
            EMethod (transpile e) i (transpile <$> exprs)
        Abs.ENull _ (Abs.Ident i) -> ENull i
        Abs.Neg _ expr -> Neg (transpile expr)
        Abs.Not _ expr -> Not (transpile expr)
        Abs.EMul _ lExpr op rExpr -> transpileThree EMul lExpr op rExpr
        Abs.EAdd _ lExpr op rExpr -> transpileThree EAdd lExpr op rExpr
        Abs.ERel _ lExpr op rExpr -> transpileThree ERel lExpr op rExpr
        Abs.EAnd _ lExpr rExpr -> transpileTwo EAnd lExpr rExpr
        Abs.EOr _ lExpr rExpr -> transpileTwo EOr lExpr rExpr

instance Transpilable (Abs.AddOp a) AddOp where
    transpile (Abs.Plus _)  = Plus
    transpile (Abs.Minus _) = Minus

instance Transpilable (Abs.MulOp a) MulOp where
    transpile (Abs.Times _) = Times
    transpile (Abs.Div _)   = Div
    transpile (Abs.Mod _)   = Mod

instance Transpilable (Abs.RelOp a) RelOp where
    transpile (Abs.LTH _) = LTH
    transpile (Abs.GTH _) = GTH
    transpile (Abs.EQU _) = EQU
    transpile (Abs.LE _)  = LE
    transpile (Abs.GE _)  = GE
    transpile (Abs.NE _)  = NE

transpileTwo ctr a b = 
    ctr (transpile a) (transpile b)
transpileThree ctr a b c = 
    ctr (transpile a) (transpile b) (transpile c)