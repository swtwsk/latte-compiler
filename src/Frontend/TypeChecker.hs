module Frontend.TypeChecker (
    typeCheck,
    TypeCheckResult(..),
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Applicative ((<*>))
import Control.Monad (unless, void)
import Data.List (foldl')
import Data.Bifunctor (bimap)

import AST.AbsLatte
import Utils.ConstantExpressions (evaluateBool)
import Frontend.Exceptions
import Frontend.ReturnType
import qualified Frontend.AST as FAST
import qualified Frontend.TranspileAST as TAST

type TypeU = Type ()
data TypeM = TypeM { _type :: TypeU, _outer :: Bool }
type PosType = Maybe (Int, Int)
type ReturnTypeU = ReturnType TypeU

type StateMap = Map.Map String TypeM
type StateErr = (PosType, FrontendException ())
type TCState  = StateT StateMap (Except StateErr)
type TCReader = ReaderT StateMap (Except StateErr)

data TypeCheckResult = GoodChecked FAST.Program 
                     | BadChecked (PosType, FrontendException ())

instance Show TypeCheckResult where
    show (GoodChecked newAst) = show newAst
    show (BadChecked (m, err)) = case m of
        Nothing -> show err
        Just (line, col) -> show line ++ ":" ++ show col ++ ": " ++ show err

typeM :: TypeU -> TypeM
typeM t = TypeM { _type = t, _outer = False }

library :: StateMap
library = Map.fromList [ ("printInt", typeM $ fun voidT [int])
                       , ("printString", typeM $ fun voidT [string])
                       , ("error", typeM $ fun voidT []) 
                       , ("readInt", typeM $ fun int [])
                       , ("readString", typeM $ fun string []) ]
    where
        fun = Fun ()
        int = Int ()
        string = Str ()
        voidT = Void ()

typeCheck :: Program PosType -> TypeCheckResult
typeCheck p@(Program _ topdefs) = case Map.lookup "main" fundefs of
    Nothing -> BadChecked (Nothing, NoMain)
    Just mainType -> if _type mainType /= Fun () (Int ()) []
        then BadChecked (Nothing, NoMain)
        else either BadChecked (const good) $
            runExcept (runReaderT (forM_ topdefs checkTopDef) fundefs)
    where good = GoodChecked $ TAST.transpile p
          fundefs = foldl' insertFunDef library (map void topdefs)
          insertFunDef acc (FnDef _ t (Ident i) args _) = 
            let argTs = foldr (\(Arg _ at _) -> (void at :)) [] args
                newT  = typeM $ Fun () t argTs
            in Map.insert i newT acc

checkTopDef :: TopDef PosType -> TCReader ()
checkTopDef (FnDef pos t (Ident i) args block) = do
    foldM_ unique Set.empty $ fmap fst argsWithT
    newEnv <- asks (Map.union (Map.fromList argsWithT))
    bType <- either throwError return $ evalTCState (checkBlock block) newEnv
    case bType of
        NoReturn -> unless (t' == Void ()) throwNoRet
        c@(ConstantReturn t'') -> unless (t' == t'') $ throwError (typeErr t'')
        r@(Return t'') -> unless (t' == t'') $ throwError (typeErr t'')
    where
        t' = void t
        extractArg (Arg _ at (Ident i)) = (i, typeM $ void at)
        argsWithT = foldr ((:) . extractArg) [] args
        throwNoRet = throwError (pos, NoReturnErr i)
        typeErr t'' = (pos, WrongRetType { _function = i
                                         , _expectedType = t'
                                         , _gotType = t'' })
        unique :: Set.Set String -> String -> TCReader (Set.Set String)
        unique acc el = if el `Set.member` acc 
            then throwError (pos, DuplicatedArg i el) 
            else return $ el `Set.insert` acc

checkBlock :: Block PosType -> TCState ReturnTypeU
checkBlock (Block pos stmts) = do
    checked <- forM stmts checkStmt
    let checked' = checkReturnTypes (Right NoReturn) (concat checked)
    either (\l -> throwError (pos, l)) return checked'
    where
        differentTypes t1 t2 = Left $ DifferentTypes t1 t2
        checkReturnTypes :: 
            Either (FrontendException ()) ReturnTypeU ->
            [ReturnTypeU] ->  
            Either (FrontendException ()) ReturnTypeU
        checkReturnTypes l@(Left _) _ = l
        checkReturnTypes rAcc [] = rAcc
        checkReturnTypes rAcc [h] = case (rAcc, h) of
            (Right NoReturn, r) -> Right r
            (cr@(Right (ConstantReturn _)), NoReturn) -> cr
            (_, NoReturn) -> Right NoReturn
            (Right r@(ConstantReturn t), ConstantReturn t') ->
                if t == t' then Right r else differentTypes t t'
            (Right r@(ConstantReturn t), Return t') ->
                if t == t' then Right r else differentTypes t t'
            (Right (Return t), r@(ConstantReturn t')) ->
                if t == t' then Right r else differentTypes t t'
            (Right r@(Return t), Return t') ->
                if t == t' then Right r else differentTypes t t'
        checkReturnTypes rAcc (h:t) = checkReturnTypes (case (rAcc, h) of
            (Right NoReturn, r) -> Right r
            (cr@(Right (ConstantReturn _)), NoReturn) -> cr
            (r, NoReturn) -> r
            (Right r@(ConstantReturn t), ConstantReturn t') ->
                if t == t' then Right r else differentTypes t t'
            (Right r@(ConstantReturn t), Return t') ->
                if t == t' then Right r else differentTypes t t'
            (Right (Return t), r@(ConstantReturn t')) ->
                if t == t' then Right r else differentTypes t t'
            (Right r@(Return t), Return t') ->
                if t == t' then Right r else differentTypes t t') t

checkStmt :: Stmt PosType -> TCState [ReturnTypeU]
checkStmt (Empty _) = return . pure $ NoReturn
checkStmt (BStmt _ block) = do
    env <- gets (Map.map (\v -> v {_outer = True}))
    either throwError (return . pure) $ evalTCState (checkBlock block) env
checkStmt (Decl pos t items) = do
    let foldF acc el = do
            i  <- extractIdent el
            v  <- gets (Map.lookup i)
            i' <- maybe (return i) (\jv -> if _outer jv 
                then return i else throwError (pos, Redefinition i)) v
            return $ (i, typeM t'):acc
    typedItems <- foldM foldF [] items
    let typedItemsMap = Map.fromList typedItems
    modify (Map.union typedItemsMap)
    return [NoReturn]
    where
        t' = void t
        extractIdent :: Item PosType -> TCState String
        extractIdent (NoInit _ (Ident i)) = return i
        extractIdent (Init pos (Ident i) expr) = do
            env <- get
            let err = evalTCState (checkExpr expr) env
            either throwError (\et -> if et == t' 
                then return i 
                else throwError (pos, ExprType { _expr = Right (void expr)
                                               , _expectedType = t'
                                               , _gotType = et })) err
checkStmt (Ass pos (Ident i) expr) = do
    iType <- lookupOrThrow i (pos, UndeclaredVar i)
    exprType <- checkExpr expr
    when (exprType /= iType) $ throwError (err iType exprType)
    return [NoReturn]
    where
        err it et = (pos, ExprType { _expr = Right (void expr)
                                   , _expectedType = it
                                   , _gotType = et })
checkStmt (Incr pos (Ident i)) = checkIncrExpr i pos
checkStmt (Decr pos (Ident i)) = checkIncrExpr i pos
checkStmt (Ret _ expr) = checkExpr expr >>= \t -> return [Return t]
checkStmt (VRet _) = return . pure $ Return (Void ())
checkStmt (Cond pos expr stmt) = checkOneBranchCond pos expr stmt
checkStmt (CondElse pos expr s1 s2) = do
    eType <- checkExpr expr
    when (eType /= Bool ()) $
        throwError (pos, ExprType { _expr = Right (void expr)
                                  , _expectedType = Bool ()
                                  , _gotType = eType })
    rs1 <- checkStmt s1
    rs2 <- checkStmt s2
    case evaluateBool expr of
        Nothing -> return $ returnType NoReturn Return Return <$> rs1
        Just True -> return $
            returnType NoReturn ConstantReturn ConstantReturn <$> rs1
        Just False -> return $
            returnType NoReturn ConstantReturn ConstantReturn <$> rs2
checkStmt (While pos expr stmt) = checkOneBranchCond pos expr stmt
checkStmt (SExp _ expr) = checkExpr expr >> return [NoReturn]

checkOneBranchCond pos expr stmt = do
    eType <- checkExpr expr
    when (eType /= Bool ()) $ 
        throwError (pos, ExprType { _expr = Right (void expr)
                                  , _expectedType = Bool ()
                                  , _gotType = eType })
    retType <- checkStmt stmt
    return . (++ [NoReturn]) $ case evaluateBool expr of
        Nothing -> returnType NoReturn Return Return <$> retType
        Just True -> 
            returnType NoReturn ConstantReturn ConstantReturn <$> retType
        Just False -> 
            returnType NoReturn Return Return <$> retType

checkExpr :: Expr PosType -> TCState TypeU
checkExpr (EVar pos (Ident i)) = lookupOrThrow i (pos, UndeclaredVar i)
checkExpr (ELitInt _ _) = justType Int
checkExpr (ELitTrue _)  = justType Bool
checkExpr (ELitFalse _) = justType Bool
checkExpr (EApp pos (Ident i) exprs) = do
    fType <- lookupOrThrow i (pos, UndeclaredVar i)
    (retType, argTypes) <- case fType of
        (Fun _ ret args) -> return (ret, args)
        _ -> throwError (pos, NotAFunc i)
    when (length argTypes /= length exprs) $ 
        throwError (pos, ArgCount { _function = i
                                  , _expectedCount = length argTypes
                                  , _gotCount = length exprs })
    env <- get
    let typedExprs = zip3 argTypes exprs [1..]
        foldF (t, e, i') = do
            t'  <- evalTCState (checkExpr e) env
            if t == t' then Right ()
            else Left (pos, NthArgument { _index = i'
                                        , _function = i
                                        , _expectedType = t
                                        , _gotType = t' })
        checked = foldr (\e -> (foldF e :)) [] typedExprs
    either throwError (constReturn retType) (sequence checked)
checkExpr (EString _ _) = justType Str
checkExpr (Neg pos expr)  = do
    exprType <- checkExpr expr
    case exprType of
        Int _ -> justType Int
        _ -> throwError (pos, ExprType { _expr = Right (void expr)
                                       , _expectedType = Int ()
                                       , _gotType = exprType })
checkExpr (Not pos expr)  = do
    exprType <- checkExpr expr
    case exprType of
        Bool _ -> justType Bool
        _ -> throwError (pos, ExprType { _expr = Right (void expr)
                                       , _expectedType = Bool ()
                                       , _gotType = exprType })
checkExpr (EMul pos e1 _ e2) = checkIntExpr e1 e2 pos
checkExpr (EAdd pos e1 op e2) = case op of
    Minus _ -> checkIntExpr e1 e2 pos
    Plus _  -> do
        et1 <- checkExpr e1
        et2 <- checkExpr e2
        when (et1 /= et2) (throwError (pos, CannotAdd et1 et2))
        case et1 of
            Int _ -> justType Int
            Str _ -> justType Str
            _ -> throwError (pos, CannotAdd et1 et2)
checkExpr (ERel pos e1 op e2) = case op of
    EQU _ -> checkEQExpr e1 e2 pos
    NE _  -> checkEQExpr e1 e2 pos
    _ -> checkIntExpr e1 e2 pos >> justType Bool
checkExpr (EAnd pos e1 e2)   = checkBoolExpr e1 e2 pos
checkExpr (EOr pos e1 e2)    = checkBoolExpr e1 e2 pos

checkEQExpr :: Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkEQExpr e1 e2 pos = do
    te1 <- checkExpr e1
    te2 <- checkExpr e2
    (checkIntExpr e1 e2 pos `catchError` 
        (\_ -> checkBoolExpr e1 e2 pos)) `catchError` 
        (\_ -> throwError (pos, EqualityErr te1 te2))
    justType Bool

checkIncrExpr :: String -> PosType -> TCState [ReturnTypeU]
checkIncrExpr i pos = do
    iType <- lookupOrThrow i (pos, UndeclaredVar i)
    when (iType /= Int ()) $ 
        throwError (pos, ExprType { _expr = Left i
                                  , _expectedType = Int ()
                                  , _gotType = iType })
    return [NoReturn]

checkIntExpr :: Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkIntExpr = checkTypedExpr (Int ())

checkBoolExpr :: Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkBoolExpr = checkTypedExpr (Bool ())

checkTypedExpr :: TypeU -> Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkTypedExpr t e1 e2 pos = do
    et1 <- checkExpr e1
    et2 <- checkExpr e2
    if et1 == et2 && et1 == t then return t
    else throwError (pos, ExprsTypes { _expr1 = Right (void e1)
                                     , _expr2 = Right (void e2)
                                     , _expectedType = t
                                     , _gotType1 = et1
                                     , _gotType2 = et2 })

justType :: (() -> Type ()) -> TCState TypeU
justType t = return $ t ()

evalTCState :: TCState a -> StateMap -> Either StateErr a
evalTCState f stateMap = runExcept (evalStateT f stateMap)

lookupOrThrow :: String -> StateErr -> TCState TypeU
lookupOrThrow k err = do
    v <- gets (Map.lookup k)
    maybe (throwError err) (return . _type) v

constReturn :: Monad m => a -> b -> m a
constReturn x = const (return x)
