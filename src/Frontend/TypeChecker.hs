module Frontend.TypeChecker (
    typeCheck,
    TypeCheckResult(..),
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Either (isLeft)
import Data.Bifunctor (bimap)

import AST.AbsLatte

type TypeU = Type ()
data TypeM = TypeM { _type :: TypeU, _outer :: Bool }

type StateMap = Map.Map String TypeM
type StateErr = (Maybe (Int, Int), String)
type TCState  = StateT StateMap (Except StateErr)
type TCReader = ReaderT StateMap (Except StateErr)

type PosType = Maybe (Int, Int)

data TypeCheckResult = GoodChecked | BadChecked StateErr

instance Show TypeCheckResult where
    show GoodChecked = "Good"
    show (BadChecked (m, err)) = show m ++ " - " ++ err

typeM :: TypeU -> TypeM
typeM t = TypeM { _type = t, _outer = False }

library :: StateMap
library = Map.fromList [ ("printInt", typeM $ fun voidT [int])
                       , ("printString", typeM $ fun voidT [string])
                       , ("error", typeM $ fun voidT []) 
                       , ("readInt", typeM $ fun int [])
                       , ("readString", typeM $ fun string []) ]
    where
        fun a b = Fun () a b
        int = Int ()
        string = Str ()
        voidT = Void ()

typeCheck :: Program PosType -> TypeCheckResult
typeCheck (Program _ topdefs) = either bad (const GoodChecked) $
    runExcept (runReaderT (forM_ topdefs checkTopDef) fundefs)
    where bad = BadChecked . (bimap id ("Typecheck error: " ++))
          fundefs = foldl' insertFunDef library (map unitType topdefs)
          insertFunDef acc (FnDef _ t (Ident i) args _) = 
            let argTs = foldr (\(Arg _ at _) -> ((unitType at):)) [] args
                newT  = typeM $ Fun () t argTs
            in Map.insert i newT acc

checkTopDef :: TopDef PosType -> TCReader ()
checkTopDef (FnDef pos t (Ident i) args block) = do
    unique $ fmap fst argsWithT
    newEnv <- asks (Map.union (Map.fromList argsWithT))
    bType <- either throwError return $ evalTCState (checkBlock block) newEnv
    maybe throwNoRet (\t'' -> if t' == t'' then return () else throwError (typeErr t'')) bType
    where
        t' = unitType t
        extractArg (Arg _ at (Ident i)) = (i, typeM $ unitType at)
        argsWithT = foldr ((:) . extractArg) [] args
        throwNoRet = case t' of
            Void _ -> return ()
            _ -> throwError (pos, "No return inside function " ++ i)
        typeErr t'' = (pos, "Expected function " ++ i ++ " to have a type " ++ show t' ++ ", but a return inside it returns type " ++ show t'')
        unique :: [String] -> TCReader ()
        unique l = foldM_ (\acc el -> if el `Set.member` acc then throwError (pos, "Duplicated argument " ++ el ++ " in function " ++ i) else return $ el `Set.insert` acc) Set.empty l

checkBlock :: Block PosType -> TCState (Maybe TypeU)
checkBlock (Block pos stmts) = do
    checked <- forM stmts checkStmt
    let checked' = foldl' (\acc el -> case el of
            Nothing -> acc
            Just t -> case acc of
                Right Nothing -> Right $ Just t
                r@(Right (Just t')) -> if t == t' then r else Left "Wrong return types in block"
                l@(Left _) -> l) (Right Nothing) checked
    either (\l -> throwError (pos, l)) return checked'

checkStmt :: (Stmt PosType) -> TCState (Maybe TypeU)
checkStmt (Empty _) = return Nothing
checkStmt (BStmt _ block) = do
    env <- gets (Map.map (\v -> v {_outer = True}))
    either throwError return $ evalTCState (checkBlock block) env
checkStmt (Decl pos t items) = do
    let foldF = \acc el -> do
            i  <- extractIdent el
            v  <- gets (Map.lookup i)
            i' <- maybe (return i) (\jv -> if _outer jv then return i else throwError (pos, "Redefinition of variable " ++ i)) v
            return $ (i, typeM $ t'):acc
    typedItems <- foldM foldF [] items
    let typedItemsMap = Map.fromList typedItems
    modify (Map.union typedItemsMap)
    return Nothing
    where
        t' = unitType t
        extractIdent :: Item PosType -> TCState String
        extractIdent (NoInit _ (Ident i)) = return i
        extractIdent (Init pos (Ident i) expr) = do
            env <- get
            let err = evalTCState (checkExpr expr) env
            flip (either throwError) err $ (\et -> if et == t' then return i else throwError (pos, "Expected expression " ++ show expr ++ " to have type " ++ show t' ++ ", but it has type " ++ show et))
checkStmt (Ass pos (Ident i) expr) = do
    iType <- lookupOrThrow i lookupErr
    exprType <- checkExpr expr
    when (exprType /= iType) $ throwError (err iType exprType)
    return Nothing
    where
        lookupErr = (pos, "Undeclared variable " ++ i)
        err it et = (pos, "Expected expression " ++ show expr ++ " to have type " ++ show it ++ ", but it has type " ++ show et)
checkStmt (Incr pos (Ident i)) = do
    iType <- lookupOrThrow i lookupErr
    when (iType /= Int ()) $ throwError (pos, i ++ " has type " ++ show iType ++ ", when is expected to have " ++ show (Int ()))
    return Nothing
    where
        lookupErr = (pos, "Undeclared variable " ++ i)
checkStmt (Decr pos (Ident i)) = do
    iType <- lookupOrThrow i lookupErr
    when (iType /= Int ()) $ throwError (pos, i ++ " has type " ++ show iType ++ ", when is expected to have " ++ show (Int ())) 
    return Nothing
    where
        lookupErr = (pos, "Undeclared variable " ++ i)
checkStmt (Ret _ expr) = checkExpr expr >>= return . Just
checkStmt (VRet _) = return . Just $ Void ()
checkStmt (Cond pos expr stmt) = do
    eType <- checkExpr expr
    when (eType /= Bool ()) $ throwError (pos, show expr ++ " has type " ++ show eType ++ ", when is expected to have " ++ show (Bool ()))
    checkStmt stmt
checkStmt (CondElse pos expr s1 s2) = do
    eType <- checkExpr expr
    when (eType /= Bool ()) $ throwError (pos, show expr ++ " has type " ++ show eType ++ ", when is expected to have " ++ show (Bool ()))
    checkStmt s1
    checkStmt s2
checkStmt (While pos expr stmt) = do
    eType <- checkExpr expr
    when (eType /= Bool ()) $ throwError (pos, show expr ++ " has type " ++ show eType ++ ", when is expected to have " ++ show (Bool ()))
    checkStmt stmt
checkStmt (SExp _ expr) = checkExpr expr >> return Nothing

checkExpr :: (Expr PosType) -> TCState (TypeU)
checkExpr (EVar pos (Ident i)) =
    lookupOrThrow i (pos, "Undeclared variable " ++ i)
checkExpr (ELitInt _ _) = justType Int
checkExpr (ELitTrue _)  = justType Bool
checkExpr (ELitFalse _) = justType Bool
checkExpr (EApp pos (Ident i) exprs) = do
    fType <- lookupOrThrow i (pos, "Undeclared variable " ++ i)
    (retType, argTypes) <- case fType of
        (Fun _ ret args) -> return (ret, args)
        _ -> throwError (pos, i ++ " is not a function and cannot be applied")
    when (length argTypes /= length exprs) $ 
        throwError (pos, "Wrong number of arguments of function " ++ i)
    env <- get
    let typedExprs = zip3 argTypes exprs [1..]
        foldF = \(t, e, i) -> do
            t'  <- (evalTCState (checkExpr e) env)
            if t == t' then Right ()
            else Left $ (pos, nthErr i t t')
        checked = foldr (\e -> (foldF e :)) [] typedExprs
    either throwError (constReturn retType) (sequence checked)
    where
        -- TODO: 1st, 2nd, 3rd, 4th, etc.
        nthErr ind expected got = "Wrong type of " ++ show ind ++ "th argument. Expected " ++ show expected ++ ", got " ++ show got
checkExpr (EString _ _) = justType Str
checkExpr (Neg pos expr)  = do
    exprType <- checkExpr expr
    case exprType of
        Int _ -> justType Int
        _ -> throwError (pos, "Expected expression " ++ show expr ++ " to have type " ++ show (Int ()) ++ ", but it has type " ++ show exprType)
checkExpr (Not pos expr)  = do
    exprType <- checkExpr expr
    case exprType of
        Bool _ -> justType Bool
        _ -> throwError (pos, "Expected expression " ++ show expr ++ " to have type " ++ show (Int ()) ++ ", but it has type " ++ show exprType)
checkExpr (EMul pos e1 _ e2) = checkIntExpr e1 e2 pos
checkExpr (EAdd pos e1 op e2) = case op of
    Minus _ -> checkIntExpr e1 e2 pos
    Plus _  -> do
        et1 <- checkExpr e1
        et2 <- checkExpr e2
        when (et1 /= et2) (throwError (pos, "Expressions " ++ show e1 ++ " and " ++ show e2 ++ " doesn't have same type; they have " ++ show et1 ++ " and " ++ show et2 ++ " respectively"))
        case et1 of
            Int _ -> justType Int
            Str _ -> justType Str
            _ -> throwError (pos, "Cannot add/concat expressions of type " ++ show et1)
checkExpr (ERel pos e1 op e2) = case op of
    EQU _ -> do
        te1 <- checkExpr e1
        te2 <- checkExpr e2
        (checkIntExpr e1 e2 pos `catchError` (\_ -> checkBoolExpr e1 e2 pos)) `catchError` (\_ -> throwError (pos, "Cannot check equality of types " ++ show te1 ++ " and " ++ show te2)) >> justType Bool
    NE _  -> do
        te1 <- checkExpr e1
        te2 <- checkExpr e2
        (checkIntExpr e1 e2 pos `catchError` (\_ -> checkBoolExpr e1 e2 pos)) `catchError` (\_ -> throwError (pos, "Cannot check equality of types " ++ show te1 ++ " and " ++ show te2)) >> justType Bool
    _ -> checkIntExpr e1 e2 pos >> justType Bool
checkExpr (EAnd pos e1 e2)   = checkBoolExpr e1 e2 pos
checkExpr (EOr pos e1 e2)    = checkBoolExpr e1 e2 pos

checkIntExpr :: Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkIntExpr = checkTypedExpr (Int ())

checkBoolExpr :: Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkBoolExpr = checkTypedExpr (Bool ())

checkTypedExpr :: TypeU -> Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkTypedExpr t e1 e2 pos = do
    et1 <- checkExpr e1
    et2 <- checkExpr e2
    if et1 == et2 && et1 == t then return t else throwError (pos, "Expected expressions " ++ show e1 ++ " and " ++ show e2 ++ " to have type " ++ show t ++ ", but they have types " ++ show et1 ++ " and " ++ show et2)

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

unitType :: (Functor a) => a b -> a ()
unitType = fmap (const ())
