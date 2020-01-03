module Backend.QuadrupleGenerator (generate) where

-- import Control.Monad.State
-- import Control.Monad.Reader
-- import Control.Monad.Writer
-- import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Identity

import Data.DList (DList, singleton, toList)
import qualified Data.Map as Map

-- import Backend.VarSupply
import Frontend.AST
import Backend.Quadruples

type ReaderEnv = Map.Map String () -- nie wiem xD
data StateEnv  = StateEnv { _varSupply :: [String]
                          , _nextLabel :: Int }
type WrtList   = DList Quadruple

type GenState = RWS ReaderEnv WrtList StateEnv -- VarSupply -- Identity

generate :: Program -> [Quadruple]
generate prog@(Program topdefs) = toList wrtList
    where
        (_, wrtList) = evalRWS (processProg prog) initReader initState
        -- (_, wrtList) = flip evalVarSupply supp $ 
        --     evalRWST (processProg prog) initReader initState
        initReader = Map.empty
        initState  = StateEnv { _varSupply = supp, _nextLabel = 0 }
        supp = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

processProg :: Program -> GenState ()
processProg (Program topdefs) = forM_ topdefs processTopDef

processTopDef :: TopDef -> GenState ()
processTopDef (FnDef t name args block) = do
    output (FunHead t name args)
    processBlock block

processArg :: Arg -> GenState ()
processArg = undefined

processBlock :: Block -> GenState ()
processBlock (Block stmts) = forM_ stmts processStmt

processStmt :: Stmt -> GenState ()
processStmt Empty = return ()
processStmt (BStmt block) = processBlock block
processStmt (Decl _ items) = forM_ items processItem
processStmt (Ass s expr) = do
    tmp <- processExpr expr
    output $ Assign Nothing (Var s) tmp
processStmt (Incr s) = output $ Binary Nothing (Var s) (Var s) BPlus (CInt 1)
processStmt (Decr s) = output $ Binary Nothing (Var s) (Var s) BMinus (CInt 1)
processStmt (Ret expr) = do
    tmp <- processExpr expr
    output $ Return Nothing (Just tmp)
processStmt (VRet) = output $ Return Nothing Nothing
processStmt (Cond expr stmt) = do
    tmp <- processExpr expr
    l1  <- nextLabel
    l2  <- nextLabel
    output $ IfJmp Nothing tmp l1 l2
    output (Label l1) >> processStmt stmt
    output $ Label l2  -- i think?
processStmt (CondElse expr s1 s2) = do
    tmp <- processExpr expr
    l1  <- nextLabel
    l2  <- nextLabel
    output $ IfJmp Nothing tmp l1 l2
    output (Label l1) >> processStmt s1
    output (Label l2) >> processStmt s2
    -- l3  <- nextLabel
    -- output $ Label l3  -- I think, again?
processStmt (While expr stmt) = do
    l1   <- nextLabel
    l2   <- nextLabel
    lEnd <- nextLabel
    output $ Goto Nothing l2
    output (Label l1) >> processStmt stmt
    output (Label l2)
    tmp <- processExpr expr
    output $ IfJmp Nothing tmp l1 lEnd
    output $ Label lEnd
processStmt (SExp expr) = processExpr expr >> return ()  -- ?

processItem :: Item -> GenState ()
processItem (NoInit s) = return ()
processItem (Init s expr) = do
    tmp <- processExpr expr
    output $ Assign Nothing (Var s) tmp

processType :: Type -> GenState ()
processType = undefined

processExpr :: Expr -> GenState Var
processExpr (EVar i) = return $ Var i
processExpr (ELitInt i) = return $ CInt i
processExpr ELitTrue = return $ CBool True
processExpr ELitFalse = return $ CBool False
processExpr (EApp fname exprs) = do
    elist <- forM exprs processExpr
    forM_ elist (output . (Param Nothing))
    t <- nextVar
    output $ FCall Nothing t fname (length elist)
    return t
processExpr (EString s) = return $ CString s
processExpr (Neg expr) = processUnExpr expr UMinus
processExpr (Not expr) = processUnExpr expr UNot
processExpr (EMul e1 op e2) = processBinExpr e1 e2 (getOp op)
    where
        getOp Times = BTimes
        getOp Div   = BDiv
        getOp Mod   = BMod
processExpr (EAdd e1 op e2) = processBinExpr e1 e2 (getOp op)
    where
        getOp Plus  = BPlus
        getOp Minus = BMinus
processExpr (ERel e1 op e2) = processBinExpr e1 e2 (getOp op)
    where
        getOp LTH = BLTH
        getOp LE =  BLE
        getOp GTH = BGTH
        getOp GE =  BGE
        getOp EQU = BEQU
        getOp NE = BNE
processExpr (EAnd e1 e2) = processBinExpr e1 e2 BAnd
processExpr (EOr e1 e2) = processBinExpr e1 e2 BOr

processUnExpr :: Expr -> OpUn -> GenState Var
processUnExpr e op = do
    a <- processExpr e
    t <- nextVar
    output $ Unary Nothing t op a
    return t

processBinExpr :: Expr -> Expr -> OpBin -> GenState Var
processBinExpr e1 e2 op = do
    a1 <- processExpr e1
    a2 <- processExpr e2
    t  <- nextVar
    output $ Binary Nothing t a1 op a2
    return t

processAddOp :: AddOp -> GenState ()
processAddOp = undefined

processMulOp :: MulOp -> GenState ()
processMulOp = undefined

processRelOp :: RelOp -> GenState ()
processRelOp = undefined

-- helper functions
output :: Quadruple -> GenState ()
output x = tell $ singleton x

nextLabel :: GenState String
nextLabel = do
    state <- get
    let ind = _nextLabel state
    put $ state { _nextLabel = ind + 1 }
    return $ "L" ++ show ind

nextVar :: GenState Var
nextVar = do
    state <- get
    let (x, xs) = fromInfiniteList $ _varSupply state
    put $ state { _varSupply = xs }
    return . Var $ "%t_" ++ x

-- new GHC version fix, courtesy of haskell-chart repository
-- https://github.com/timbod7/haskell-chart/pull/197
fromInfiniteList :: [a] -> (a, [a])
fromInfiniteList []     = error "VarSupply: empty list"
fromInfiniteList (x:xs) = (x, xs)

-- nextIndex :: GenState Int
-- nextIndex = do
--     state <- get
--     let ind = _next state
--     put $ state { _next = ind + 1 }
--     return ind

insertIfNot :: (Ord k) => k -> v -> Map.Map k v -> Map.Map k v
insertIfNot key value map = case Map.lookup key map of
    Nothing -> Map.insert key value map
    Just _  -> map

-- outputIndented :: String -> GenState ()
-- outputIndented = tell . singleton . indent

-- indent :: String -> String
-- indent s = "    " ++ s
