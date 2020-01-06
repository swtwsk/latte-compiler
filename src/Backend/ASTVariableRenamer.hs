module Backend.ASTVariableRenamer (renameNestedVariables) where

import qualified Data.Map.Strict as Map
import Control.Monad.State

import Frontend.AST
import Backend.VarSupply

type RenameMap = Map.Map String String
type Supply = [String]
type RenameState = StateT RenameMap VarSupply

runRename :: RenameState a -> RenameMap -> Supply -> (a, Supply)
runRename f rm = runVarSupply (evalStateT f rm)

renameNestedVariables :: Program -> Program
renameNestedVariables (Program topdefs) = Program $ processTopDef <$> topdefs

processTopDef :: TopDef -> TopDef
processTopDef (FnDef t s args block) = FnDef t s args res    
    where
        (res, _) = runRename (processBlock block) argsMap supp
        supp = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence
        extractArg (Arg _ i) = (i, i)
        argsMap = Map.fromList $ foldr ((:) . extractArg) [] args

processBlock :: Block -> RenameState Block
processBlock (Block stmts) = fmap Block (forM stmts processStmt)

processStmt :: Stmt -> RenameState Stmt
processStmt Empty = return Empty
processStmt (BStmt block) = do
    env  <- get
    supp <- getSupply
    let (res, newSupp) = runRename (processBlock block) env supp
    putSupply newSupp
    return $ BStmt res
processStmt (Decl t items) = do
    items' <- forM items processItem
    return $ Decl t items'
processStmt (Ass s expr) = do
    s' <- gets $ flip (Map.!) s
    e' <- processExpr expr
    return $ Ass s' e'
processStmt (Incr s) = fmap Incr (varFromIdent s)
processStmt (Decr s) = fmap Decr (varFromIdent s)
processStmt (Ret expr) = fmap Ret (processExpr expr)
processStmt VRet = return VRet
processStmt (Cond expr stmt) = processCond Cond expr stmt
processStmt (CondElse expr stmt1 stmt2) = do
    expr' <- processExpr expr
    stmt1' <- processStmt stmt1
    stmt2' <- processStmt stmt2
    return $ CondElse expr' stmt1' stmt2'
processStmt (While expr stmt) = processCond While expr stmt
processStmt (SExp expr) = fmap SExp (processExpr expr)

processItem :: Item -> RenameState Item
processItem (NoInit s) = fmap NoInit (processItemVar s)
processItem (Init s e) = do
    e' <- processExpr e
    s' <- processItemVar s
    return $ Init s' e'

processExpr :: Expr -> RenameState Expr
processExpr (EVar s) = fmap EVar (varFromIdent s)
processExpr (EApp s exprs) = do
    exprs' <- forM exprs processExpr
    return $ EApp s exprs'
processExpr (Neg expr) = fmap Neg (processExpr expr)
processExpr (Not expr) = fmap Not (processExpr expr)
processExpr (EMul expr1 op expr2) = processBinExpr EMul expr1 op expr2
processExpr (EAdd expr1 op expr2) = processBinExpr EAdd expr1 op expr2
processExpr (ERel expr1 op expr2) = processBinExpr ERel expr1 op expr2
processExpr (EAnd expr1 expr2) = processLogicalExpr EAnd expr1 expr2
processExpr (EOr expr1 expr2) = processLogicalExpr EOr expr1 expr2
-- ELitInt, ELitTrue, ELitFalse, EString
processExpr e = return e

processItemVar :: String -> RenameState String
processItemVar s = do
    renMap <- get
    newVar <- maybe (return s) (const $ buildVar varBuilder) $
        Map.lookup s renMap
    put $ Map.insert s newVar renMap
    return newVar
    where
        varBuilder v = "#" ++ s ++ "_" ++ v

varFromIdent :: String -> RenameState String
varFromIdent s = gets $ flip (Map.!) s

processCond :: (Expr -> Stmt -> Stmt) -> Expr -> Stmt -> RenameState Stmt
processCond ctr expr stmt = do
    expr' <- processExpr expr
    stmt' <- processStmt stmt
    return $ ctr expr' stmt'

processBinExpr :: (Expr -> a -> Expr -> Expr) -> 
    Expr -> a -> Expr -> RenameState Expr
processBinExpr ctr e1 op e2 = do
    e1' <- processExpr e1
    e2' <- processExpr e2
    return $ ctr e1' op e2'

processLogicalExpr :: (Expr -> Expr -> Expr) -> Expr -> Expr -> RenameState Expr
processLogicalExpr ctr e1 e2 = do
    e1' <- processExpr e1
    e2' <- processExpr e2
    return $ ctr e1' e2'
