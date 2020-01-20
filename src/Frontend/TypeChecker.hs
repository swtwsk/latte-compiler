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

import Lens.Micro.Platform ((^.), (%=), over)

import AST.AbsLatte
import Utils.ConstantExpressions (evaluateBool)
import Frontend.Exceptions
import Frontend.ReturnType
import Frontend.TypeCheckerStructs
import qualified Frontend.AST as FAST
import qualified Frontend.TranspileAST as TAST

import Globals

type TCState  = StateT StateTuple (Except StateErr)
type TCReader = ReaderT StateTuple (Except StateErr)

data TypeCheckResult = GoodChecked FAST.Program 
                     | BadChecked StateErr

instance Show TypeCheckResult where
    show (GoodChecked newAst) = show newAst
    show (BadChecked (m, err)) = case m of
        Nothing -> show err
        Just (line, col) -> show line ++ ":" ++ show col ++ ": " ++ show err

library :: FuncMap
library = Map.fromList [ (printIntName, typeM $ fun voidT [int])
                       , (printStringName, typeM $ fun voidT [string])
                       , (errorName, typeM $ fun voidT []) 
                       , (readIntName, typeM $ fun int [])
                       , (readStringName, typeM $ fun string []) ]
    where
        fun = Fun ()
        int = Int ()
        string = Str ()
        voidT = Void ()

typeCheck :: Program PosType -> TypeCheckResult
typeCheck p@(Program _ topdefs) = either BadChecked (const good) $ do
    mapPair <- foldl' accumulateTopDefs seed topdefs
    let (fundefs, classdefs) = mapPair
        typenames = Set.fromList $ 
            (\i -> Class () (Ident i)) <$> Map.keys classdefs
        stateTuple = StateTuple { _funcs   = fundefs
                                , _classes = classdefs 
                                , _types   = typenames }
    mainType <- maybe (throwError (Nothing, NoMain)) pure $ 
        Map.lookup "main" fundefs
    when (_type mainType /= Fun () (Int ()) []) (throwError (Nothing, NoMain))
    runExcept (runReaderT (forM_ topdefs checkTopDef) stateTuple)
    where good = GoodChecked $ TAST.transpile p
          seed = pure (library, Map.empty)

accumulateTopDefs :: 
    Either StateErr (FuncMap, ClassMap) -> 
    TopDef PosType -> 
    Either StateErr (FuncMap, ClassMap)
accumulateTopDefs l@(Left {}) _ = l
accumulateTopDefs (Right (sm, cm)) topdef = case topdef of
    (FnTopDef _ fndef) -> pure (Map.union (fromFnDef fndef) sm, cm)
    (ClassDef _ (Ident i) decls) -> 
        let cd  = fromClassDef (void <$> decls)
            cm' = Map.union (Map.singleton i cd) cm
        in pure (sm, cm')
    (ClassExtDef pos (Ident i) (Ident ext) decls) -> do
        extDef <- maybe (Left err) pure $ Map.lookup ext cm
        let cd = fromClassDef (void <$> decls)
            extMet = _methods extDef
            extFld = _fields extDef
            classMet = _methods cd
            classFld = _fields cd
            cd' = ClDef { _methods = Map.union classMet extMet
                        , _fields  = Map.union classFld extFld
                        , _extends = Just ext }
            cm' = Map.union (Map.singleton i cd') cm
        pure (sm, cm')
        where
            err = (pos, NoExtension { _className = i, _expectedExt = ext })

fromFnDef :: FnDef a -> Map.Map String TypeM
fromFnDef (FnDef _ t (Ident i) args _) = 
    let argTs = foldr (\(Arg _ at _) -> (void at :)) [] args
        newT  = typeM $ Fun () (void t) argTs
    in Map.singleton i newT

fromClassDef :: [ClassDecl ()] -> ClDef
fromClassDef decls = foldl' fromClassDecl emptyClassDef decls
    where
        fromClassDecl cdef (MethodDef _ fndef) =
            let m  = _methods cdef
                m' = Map.union (fromFnDef fndef) m
            in cdef { _methods = m' }
        fromClassDecl cdef (FieldDef _ t (Ident i)) =
            let f  = _fields cdef
                f' = Map.union (Map.singleton i t) f
            in cdef { _fields = f' }
        emptyClassDef = ClDef { _methods = Map.empty
                              , _fields = Map.empty
                              , _extends = Nothing }

checkTopDef :: TopDef PosType -> TCReader ()
checkTopDef (FnTopDef _ fndef) = checkFnDef fndef
checkTopDef (ClassExtDef pos (Ident name) (Ident ext) cdls) = do
    extFunc <- extractClassFunc ext
    clFunc  <- extractClassFunc name
    let newFunc = Map.union clFunc extFunc
    local (over funcs $ Map.union newFunc) $ mapM_ checkClassDecl cdls
checkTopDef (ClassDef _ (Ident name) cdls) = do
    newFunc <- extractClassFunc name
    local (over funcs $ Map.union newFunc) $ mapM_ checkClassDecl cdls

extractClassFunc :: String -> TCReader FuncMap
extractClassFunc name = do
    classDef <- asks ((flip (Map.!) name) . (^. classes))
    let flds = Map.map typeM $ classDef ^. fields
        mtds = classDef ^. methods
    return $ 
        Map.insert "self" (typeM $ Class () (Ident name)) (Map.union flds mtds)

checkClassDecl :: ClassDecl PosType -> TCReader ()
checkClassDecl (MethodDef _ fndef) = checkFnDef fndef
checkClassDecl (FieldDef pos t (Ident _)) = do
    types' <- asks (^. types)
    let t' = void t
    unless (isProperType types' t') (throwError (pos, TypeNotExists t'))

checkFnDef :: FnDef PosType -> TCReader ()
checkFnDef (FnDef pos t (Ident i) args block) = do
    foldM_ unique Set.empty $ fmap fst argsWithT
    newEnv <- asks (over funcs $ Map.union (Map.fromList argsWithT))
    bType <- either throwError return $ evalTCState (checkBlock block) newEnv
    case bType of
        NoReturn -> unless (t' == Void ()) throwNoRet
        ConstantReturn t'' -> unless (t' == t'') $ throwError (typeErr t'')
        Return t'' -> unless (t' == t'') $ throwError (typeErr t'')
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
    env <- gets (over funcs $ Map.map (\v -> v {_outer = True}))
    either throwError (return . pure) $ evalTCState (checkBlock block) env
checkStmt (Decl pos t items) = do
    let foldF acc el = do
            i  <- extractIdent el
            v  <- gets (Map.lookup i . (^. funcs))
            maybe (return i) (\jv -> if _outer jv 
                then return i else throwError (pos, Redefinition i)) v
            return $ (i, typeM t'):acc
    typedItems <- foldM foldF [] items
    let typedItemsMap = Map.fromList typedItems
    funcs %= (Map.union typedItemsMap)
    return [NoReturn]
    where
        t' = void t
        extractIdent :: Item PosType -> TCState String
        extractIdent (NoInit _ (Ident i)) = return i
        extractIdent (Init pos (Ident i) expr) = do
            env   <- get
            et    <- either throwError return $ evalTCState (checkExpr expr) env
            isSub <- et `isSubtypeOf` t'
            if isSub
                then return i 
                else throwError (pos, ExprType { _expr = Right (void expr)
                                               , _expectedType = t'
                                               , _gotType = et })
checkStmt (Ass pos lvalue expr) = do
    iType    <- checkLValue lvalue
    exprType <- checkExpr expr
    isSub    <- exprType `isSubtypeOf` iType
    unless isSub $ throwError (err iType exprType)
    return [NoReturn]
    where
        err it et = (pos, ExprType { _expr = Right (void expr)
                                   , _expectedType = it
                                   , _gotType = et })
checkStmt (Incr pos lvalue) = checkIncrExpr lvalue pos
checkStmt (Decr pos lvalue) = checkIncrExpr lvalue pos
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
checkStmt (For pos t (Ident i) expr stmt) = do
    eType <- checkExpr expr
    inner <- case eType of
        Array _ it -> return it
        _ -> throwError (typeErr eType)
    when (inner /= t') $ throwError (typeErr eType)
    env <- gets (over funcs $ Map.insert i (typeM t'))
    either throwError return $ evalTCState (checkStmt stmt) env
    where
        t' = void t
        typeErr et = (pos, ExprType { _expr = Right (void expr)
                                    , _expectedType = Array () t'
                                    , _gotType = et })

checkOneBranchCond :: 
    PosType -> 
    Expr PosType -> 
    Stmt PosType -> 
    TCState [ReturnTypeU]
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

checkLValue :: Expr PosType -> TCState TypeU
checkLValue e@(EVar _ (Ident i)) = checkExpr e
checkLValue e@(EArrGet {}) = checkExpr e
checkLValue e@(EFieldGet {}) = checkExpr e
checkLValue expr = throwError (extract expr, WrongLValue (void expr))

checkExpr :: Expr PosType -> TCState TypeU
checkExpr (EVar pos (Ident i)) = lookupVarOrThrow i (pos, UndeclaredVar i)
checkExpr (ELitInt _ _) = justType Int
checkExpr (ELitTrue _)  = justType Bool
checkExpr (ELitFalse _) = justType Bool
checkExpr (EApp pos (Ident i) exprs) = do
    fType <- lookupVarOrThrow i (pos, UndeclaredVar i)
    (retType, argTypes) <- case fType of
        (Fun _ ret args) -> return (ret, args)
        _ -> throwError (pos, NotAFunc i)
    when (length argTypes /= length exprs) $ 
        throwError (pos, ArgCount { _function = i
                                  , _expectedCount = length argTypes
                                  , _gotCount = length exprs })
    env <- get
    let typedExprs = zip3 argTypes exprs [1..]
    forM_ typedExprs $ \(t, e, i') -> do
        t' <- either throwError return $ evalTCState (checkExpr e) env
        isSub <- t' `isSubtypeOf` t
        unless isSub $ throwError (pos, NthArgument { _index = i'
                                                    , _function = i
                                                    , _expectedType = t
                                                    , _gotType = t' })
    return retType
checkExpr (EString _ _) = justType Str
checkExpr (EArr pos t expr) = do
    exprType <- checkExpr expr
    types' <- gets (^. types)
    let t' = void t
    unless (isProperType types' t') (throwError (pos, TypeNotExists t'))
    case exprType of
        Int _ -> return $ Array () t'
        _ -> throwError (pos, ExprType { _expr = Right (void expr)
                                       , _expectedType = Int ()
                                       , _gotType = exprType })
checkExpr (EClass pos t) = do
    types' <- gets (^. types)
    let t' = void t
    unless (Set.member t' types') (throwError (pos, TypeNotExists t'))
    return t'
checkExpr (EArrGet pos e1 e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    rt <- case t1 of
        Array _ t' -> return t'
        t' -> throwError (pos, ExprType { _expr = Right (void e1)
                                        , _expectedType = Array () t2
                                        , _gotType = t' })
    unless (t2 == Int ()) (throwError (pos, ExprType { _expr = Right (void e2)
                                                     , _expectedType = Int ()
                                                     , _gotType = t2 }))
    return rt
checkExpr (EFieldGet pos expr (Ident i)) = do
    te <- checkExpr expr
    case te of
        Array _ t' -> if i == "length" 
            then return t' 
            else throwError (pos, UndeclaredMet i)
        Class _ (Ident name) -> do
            flds <- gets ((^. fields) . (flip (Map.!) name) . (^. classes))
            maybe (throwError (pos, UndeclaredFld i)) return $ Map.lookup i flds
        _ -> throwError (pos, ValueType te)
checkExpr (EMethod pos expr (Ident i) exprs) = do
    eType <- checkExpr expr
    methodType <- case eType of
        Class _ (Ident name) -> do
            mtds <- gets ((^. methods) . (flip (Map.!) name) . (^. classes))
            maybe (throwError (pos, UndeclaredMet i)) (return . _type) $
                Map.lookup i mtds
        _ -> throwError (pos, ValueType eType)
    (retType, argTypes) <- case methodType of
        (Fun _ ret args) -> return (ret, args)
        _ -> throwError (pos, NotAFunc i)
    when (length argTypes /= length exprs) $ 
        throwError (pos, ArgCount { _function = i
                                  , _expectedCount = length argTypes
                                  , _gotCount = length exprs })
    env <- get
    let typedExprs = zip3 argTypes exprs [1..]
    forM_ typedExprs $ \(t, e, i') -> do
        t' <- either throwError return $ evalTCState (checkExpr e) env
        isSub <- t' `isSubtypeOf` t
        unless isSub $ throwError (pos, NthArgument { _index = i'
                                                    , _function = i
                                                    , _expectedType = t
                                                    , _gotType = t' })
    return retType
checkExpr (ENull pos (Ident i)) = do
    types' <- gets (^. types)
    let classType = Class () (Ident i)
    if Set.member classType types'
        then return classType
        else throwError (pos, TypeNotExists classType)
checkExpr (Neg pos expr) = do
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
    ((checkIntExpr e1 e2 pos `catchError` 
        (\_ -> checkBoolExpr e1 e2 pos)) `catchError`
        (\_ -> checkClassTypeExpr e1 e2 pos)) `catchError`
        (\_ -> throwError (pos, EqualityErr te1 te2))
    justType Bool

checkIncrExpr :: Expr PosType -> PosType -> TCState [ReturnTypeU]
checkIncrExpr lvalue pos = do
    iType <- checkLValue lvalue
    when (iType /= Int ()) $ 
        throwError (pos, ExprType { _expr = Right (void lvalue)
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

checkClassTypeExpr :: Expr PosType -> Expr PosType -> PosType -> TCState TypeU
checkClassTypeExpr e1 e2 pos = do
    et1 <- checkExpr e1
    et2 <- checkExpr e2
    case et1 of
        c@(Class _ (Ident i)) -> 
            unless (et1 == et2) (throwError (pos, EqualityErr et1 et2))
        _ -> throwError (pos, ValueType et1)
    return et1
justType :: (() -> Type ()) -> TCState TypeU
justType t = return $ t ()

evalTCState :: TCState a -> StateTuple -> Either StateErr a
evalTCState f stateTuple = runExcept (evalStateT f stateTuple)

lookupVarOrThrow :: String -> StateErr -> TCState TypeU
lookupVarOrThrow k err = do
    v <- gets (Map.lookup k . (^. funcs))
    maybe (throwError err) (return . _type) v

isProperType :: Set.Set TypeU -> TypeU -> Bool
isProperType typeSet t = case t of
    Array _ ta -> isProperType typeSet ta
    c@(Class {}) -> Set.member c typeSet
    Pointer _ t' -> isProperType typeSet t'
    _ -> True

isSubtypeOf :: TypeU -> TypeU -> TCState Bool
(Class _ (Ident name1)) `isSubtypeOf` c2@(Class _ (Ident name2)) = 
    if name1 == name2 then return True 
    else do
        ext' <- gets ((^. extends) . (flip (Map.!) name1) . (^. classes))
        case ext' of
            Nothing -> return False
            Just n' -> if n' == name2 
                then return True 
                else (Class () (Ident n')) `isSubtypeOf` c2
t1 `isSubtypeOf` t2 = return $ t1 == t2
