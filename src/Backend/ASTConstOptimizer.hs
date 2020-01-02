module Backend.ASTConstOptimizer (
    optimize, 
    optimizeExpr, 
    optimizeStmt
) where

import Frontend.AST
import Utils.ConstantExpressions

optimize :: Program -> Program
optimize p = optimizeProg p

optimizeProg :: Program -> Program
optimizeProg (Program topdefs) = Program $ fmap optimizeTopDef topdefs

optimizeTopDef :: TopDef -> TopDef
optimizeTopDef (FnDef t fname args block) =
    FnDef t fname args $ maybe (Block []) id (optimizeBlock isVoid block)
    where
        isVoid = t == TVoid

optimizeBlock :: Bool -> Block -> Maybe Block
optimizeBlock isVoid (Block stmts) =
    case catMaybes . fmap (optimizeStmt isVoid) $ stmts of
        [] -> Nothing
        l  -> Just $ Block l

optimizeStmt :: Bool -> Stmt -> Maybe Stmt
optimizeStmt isVoid stmt = case stmt of
    BStmt block -> optimizeBlock isVoid block >>= pure . BStmt
    Ass x expr -> pure $ Ass x (exprToConstLit expr)
    Ret expr -> pure $ Ret (exprToConstLit expr)
    Cond expr stmt -> optimizeOneBranchCond isVoid expr stmt
    While expr stmt -> optimizeOneBranchCond isVoid expr stmt
    CondElse expr st1 st2 -> case evaluateBool expr of
        Nothing -> do
            let e = eitherToExpr . optimizeExpr $ expr
            s1 <- optimizeStmt isVoid st1
            s2 <- optimizeStmt isVoid st2
            pure $ CondElse e s1 s2
        Just b  -> 
            if b then optimizeStmt isVoid st1 else optimizeStmt isVoid st2
    SExp expr -> pure . SExp . eitherToExpr . optimizeExpr $ expr
    -- Empty, Decl, Incr, Decr, VRet
    x -> pure x

optimizeOneBranchCond :: Bool -> Expr -> Stmt -> Maybe Stmt
optimizeOneBranchCond isVoid expr stmt = case evaluateBool expr of
    Nothing -> do
        let e = eitherToExpr $ optimizeExpr expr
        s <- optimizeStmt isVoid stmt
        pure $ Cond e s
    Just b  -> if b then optimizeStmt isVoid stmt else Nothing

-- TODO: OPTIMIZE ADD/MUL AS IT IS COMMUTATIVE
optimizeExpr :: Expr -> Either Expr CVal
optimizeExpr e = case e of
    ELitInt i -> Right $ CInt i
    ELitTrue -> Right $ CBool True
    ELitFalse -> Right $ CBool False
    EApp s exprs -> Left (EApp s $ eitherToExpr . optimizeExpr <$> exprs)
    Neg e -> case optimizeExpr e of
        Right (CInt i) -> Right (CInt (-i))
        Right r -> Left $ Neg (constToLit r)
        Left l -> Left $ Neg l
    Not e -> case optimizeExpr e of
        Right (CBool b) -> Right (CBool $ not b)
        Right r -> Left $ Not (constToLit r)
        Left l -> Left $ Not l
    EMul e1 op e2 -> case (optimizeExpr e1, optimizeExpr e2) of
        (Right (CInt i1), Right (CInt i2)) ->
            Right (CInt $ mulOp op i1 i2)
        (Right a, Right b) -> Left (EMul (constToLit a) op (constToLit b))
        (Right a, Left b)  -> Left (EMul (constToLit a) op b)
        (Left a, Right b)  -> Left (EMul a op (constToLit b))
        (Left a, Left b)   -> Left (EMul a op b)
        where
            mulOp Times = (*)
            mulOp Div = div
            mulOp Mod = mod
    EAdd e1 op e2 -> case (optimizeExpr e1, optimizeExpr e2) of
        (Right (CInt i1), Right (CInt i2)) ->
            Right (CInt $ addOp op i1 i2)
        (Right a, Right b) -> Left (EAdd (constToLit a) op (constToLit b))
        (Right a, Left b)  -> Left (EAdd (constToLit a) op b)
        (Left a, Right b)  -> Left (EAdd a op (constToLit b))
        (Left a, Left b)   -> Left (EAdd a op b)
        where
            addOp Plus  = (+)
            addOp Minus = (-)
    ERel e1 op e2 -> case (optimizeExpr e1, optimizeExpr e2) of
        (Right (CInt i1), Right (CInt i2)) -> Right (CBool $ relOp op i1 i2)
        (Right (CBool b1), Right (CBool b2)) -> Right (CBool $ relOp op b1 b2)
        (Right a, Right b) -> Left (ERel (constToLit a) op (constToLit b))
        (Right a, Left b)  -> Left (ERel (constToLit a) op b)
        (Left a, Right b)  -> Left (ERel a op (constToLit b))
        (Left a, Left b)   -> Left (ERel a op b)
        where
            relOp LTH = (<)
            relOp GTH = (>)
            relOp EQU = (==)
            relOp LE  = (<=)
            relOp GE  = (>=)
            relOp NE  = (/=)
    EAnd e1 e2 -> optimizeBoolExpr e1 e2 (&&) EAnd
    EOr e1 e2 -> optimizeBoolExpr e1 e2 (&&) EOr
    x -> Left x

optimizeBoolExpr :: 
    Expr -> 
    Expr ->
    (Bool -> Bool -> Bool) -> 
    (Expr -> Expr -> Expr) ->
    Either Expr CVal
optimizeBoolExpr e1 e2 op ctr = case (optimizeExpr e1, optimizeExpr e2) of
    (Right (CBool i1), Right (CBool i2)) ->
        Right (CBool $ i1 `op` i2)
    (Right a, Right b) -> Left (ctr (constToLit a) (constToLit b))
    (Right a, Left b)  -> Left (ctr (constToLit a) b)
    (Left a, Right b)  -> Left (ctr a (constToLit b))
    (Left a, Left b)   -> Left (ctr a b)

eitherToExpr :: Either Expr CVal -> Expr
eitherToExpr = either id constToLit

exprToConstLit :: Expr -> Expr
exprToConstLit e = maybe e constToLit $ evaluate e

constToLit :: CVal -> Expr
constToLit (CInt i) = ELitInt i
constToLit (CBool b) = if b then ELitTrue else ELitFalse

catMaybes :: [Maybe a] -> [a]
catMaybes (h:t) = case h of
    Just x  -> x : (catMaybes t)
    Nothing -> catMaybes t
catMaybes [] = []
