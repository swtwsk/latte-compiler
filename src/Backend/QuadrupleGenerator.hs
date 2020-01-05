module Backend.QuadrupleGenerator (generate) where

import Control.Monad.RWS
import Control.Monad.Identity

import Data.DList (DList, singleton, toList)
import Data.List (foldl')
import qualified Data.Map as Map

import Frontend.AST
import Backend.TypeMapConstructor
import Backend.Quadruples

data ReaderEnv = ReaderEnv { _funs      :: Map.Map String Type
                           , _varTypes  :: Map.Map String Type }
data StateEnv  = StateEnv  { _varSupply :: [String]
                           , _nextLabel :: Int }
type WrtList   = DList Quadruple

type GenState = RWS ReaderEnv WrtList StateEnv

library :: Map.Map String Type
library = Map.fromList [ ("printInt", TVoid)
                       , ("printString", TVoid)
                       , ("error", TVoid) 
                       , ("readInt", TInt)
                       , ("readString", TStr)
                       , (concatStringName, TStr) ]

generate :: Program -> [Quadruple]
generate prog@(Program topdefs) = toList wrtList
    where
        (_, wrtList) = evalRWS (processProg prog) initReader initState

        initReader = ReaderEnv { _funs = funcMap `Map.union` library
                               , _varTypes = Map.empty }
        initState  = StateEnv { _varSupply = supp
                              , _nextLabel = 0 }

        supp = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence
        funcMap = Map.fromList $ map (\(FnDef v k _ _) -> (k, v)) topdefs

processProg :: Program -> GenState ()
processProg (Program topdefs) = forM_ topdefs processTopDef

processTopDef :: TopDef -> GenState ()
processTopDef topdef@(FnDef t name args block) = do
    output (FunHead t name args)
    let varTypesMap = typeTopDef topdef
    local (\r -> r { _varTypes = varTypesMap }) $ processBlock block
    return ()

processBlock :: Block -> GenState Bool
processBlock (Block stmts) = do
    processed <- forM stmts processStmt
    return $ or processed

processStmt :: Stmt -> GenState Bool
processStmt Empty = return False
processStmt (BStmt block) = processBlock block
processStmt (Decl t items) = forM_ items (processItem t) >> return False
processStmt (Ass s expr) = do
    tmp <- processExpr expr
    output $ Assign (Var s (varType tmp)) tmp
    return False
processStmt (Incr s) = do
    output $ Binary (Var s TInt) (Var s TInt) (BAdd BPlus) (CInt 1)
    return False
processStmt (Decr s) = do
    output $ Binary (Var s TInt) (Var s TInt) (BAdd BMinus) (CInt 1)
    return False
processStmt (Ret expr) = do
    tmp <- processExpr expr
    output (Return $ Just tmp) >> return True
processStmt VRet = output (Return Nothing) >> return True
processStmt (Cond expr stmt) = do
    tmp <- processExpr expr
    l1  <- nextLabel
    l2  <- nextLabel
    output $ IfJmp tmp l1 l2
    output (Label l1) >> processStmt stmt
    output $ Label l2  -- i think?
    return False
processStmt (CondElse expr s1 s2) = do
    tmp <- processExpr expr
    l1  <- nextLabel
    l2  <- nextLabel
    l3  <- nextLabel
    output $ IfJmp tmp l1 l2
    output (Label l1)
    r1  <- processStmt s1
    unless r1 $ output (Goto l3)
    output (Label l2)
    r2 <- processStmt s2
    unless r2 $ output (Goto l3)
    let bothReturns = r1 && r2
    unless bothReturns $ output (Label l3)  -- I think, again?
    return bothReturns
processStmt (While expr stmt) = do
    l1   <- nextLabel
    l2   <- nextLabel
    lEnd <- nextLabel
    output $ Goto l2
    output (Label l1) >> processStmt stmt
    output (Label l2)
    tmp <- processExpr expr
    output $ IfJmp tmp l1 lEnd
    output $ Label lEnd
    return False
processStmt (SExp expr) = processExpr expr >> return False  -- ?

processItem :: Type -> Item -> GenState ()
processItem t (NoInit s) = output $ Assign (Var s t) defVal
    where
        defVal = case t of
            TInt -> CInt 0
            TStr -> CString ""
            TBool -> CBool False
processItem _ (Init s expr) = do
    tmp <- processExpr expr
    output $ Assign (Var s (varType tmp)) tmp

processExpr :: Expr -> GenState Var
processExpr (EVar i) = do
    t <- asks (flip (Map.!) i . _varTypes) 
    return $ Var i t
processExpr (ELitInt i) = return $ CInt i
processExpr ELitTrue = return $ CBool True
processExpr ELitFalse = return $ CBool False
processExpr (EApp fname exprs) = do
    elist <- forM exprs processExpr
    elist' <- forM elist constToTemp
    forM_ (reverse elist') (output . Param)
    ftype <- asks $ flip (Map.!) fname . _funs
    t <- nextVar ftype
    output $ (if ftype == TVoid then Call else FCall t) fname (length elist)
    return t
    where
        constToTemp :: Var -> GenState Var
        constToTemp v@Var  {} = return v
        constToTemp t@Temp {} = return t
        constToTemp c = do
            tmp <- nextVar (varType c)
            output (Assign tmp c)
            return tmp
processExpr (EString s) = return $ CString s
processExpr (Neg expr) = processUnExpr expr UMinus
processExpr (Not expr) = processUnExpr expr UNot
processExpr (EMul e1 op e2) = processBinExpr e1 e2 (BMul $ getOp op)
    where
        getOp Times = BTimes
        getOp Div   = BDiv
        getOp Mod   = BMod
processExpr (EAdd e1 Plus e2) = processAddExpr e1 e2
processExpr (EAdd e1 Minus e2) = processBinExpr e1 e2 (BAdd BMinus)
processExpr (ERel e1 op e2) = processBinExpr e1 e2 (BRel $ getOp op)
    where
        getOp LTH = BLTH
        getOp LE =  BLE
        getOp GTH = BGTH
        getOp GE =  BGE
        getOp EQU = BEQU
        getOp NE = BNE
processExpr (EAnd e1 e2) = processBinExpr e1 e2 (BLog BAnd)
processExpr (EOr e1 e2) = processBinExpr e1 e2 (BLog BOr)

processUnExpr :: Expr -> OpUn -> GenState Var
processUnExpr e op = do
    a <- processExpr e
    t <- nextVar (varType a)
    output $ Unary t op a
    return t

processBinExpr :: Expr -> Expr -> OpBin -> GenState Var
processBinExpr e1 e2 op = do
    a1 <- processExpr e1
    a2 <- processExpr e2
    t  <- nextVar (varType a1)
    output $ Binary t a1 op a2
    return t

processAddExpr :: Expr -> Expr -> GenState Var
processAddExpr e1 e2 = do
    a1 <- processExpr e1
    a2 <- processExpr e2
    t  <- nextVar (varType a1)
    case varType a1 of
        TInt -> output $ Binary t a1 (BAdd BPlus) a2
        TStr -> processExpr (EApp concatStringName 
            [varToExpr a1, varToExpr a2]) >>= (\t' -> output $ Assign t t')
    return t
    where
        varToExpr v = case v of
            Var s _ -> EVar s
            Temp s _ -> EVar s
            CInt i -> ELitInt i 
            CBool b -> if b then ELitTrue else ELitFalse 
            CString s -> EString s

-- helper functions
output :: Quadruple -> GenState ()
output x = tell $ singleton x

nextLabel :: GenState String
nextLabel = do
    state <- get
    let ind = _nextLabel state
    put $ state { _nextLabel = ind + 1 }
    return $ ".L" ++ show ind

nextVar :: Type -> GenState Var
nextVar t = do
    state <- get
    let (x, xs) = fromInfiniteList $ _varSupply state
    put $ state { _varSupply = xs }
    return $ Temp x t

concatStringName :: String
concatStringName = "__concatString"

-- new GHC version fix, courtesy of haskell-chart repository
-- https://github.com/timbod7/haskell-chart/pull/197
fromInfiniteList :: [a] -> (a, [a])
fromInfiniteList []     = error "VarSupply: empty list"
fromInfiniteList (x:xs) = (x, xs)

insertIfNot :: (Ord k) => k -> v -> Map.Map k v -> Map.Map k v
insertIfNot key value map = case Map.lookup key map of
    Nothing -> Map.insert key value map
    Just _  -> map
