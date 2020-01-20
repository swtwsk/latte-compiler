module Backend.QuadrupleGenerator (generate) where

import Control.Monad.RWS
import Control.Monad.Identity

import Data.DList (DList, singleton, toList)
import Data.List (foldl')
import Lens.Micro.Platform (Lens', lens, (^.), over)
import qualified Data.Map as Map

import qualified Data.Graph as Graph

import Frontend.AST
import Backend.TypeMapConstructor
import Backend.Quadruples
import Globals

data ReaderEnv = ReaderEnv { _funs      :: Map.Map String Type
                           , _classes   :: ClassMap
                           , _varTypes  :: Map.Map String Type
                           , _className :: Maybe String }
data StateEnv  = StateEnv  { _varSupply :: [String]
                           , _nextLabel :: Int }
type WrtList   = DList Quadruple

type GenState = RWS ReaderEnv WrtList StateEnv

type FieldMap  = Map.Map String (Int, Type)
type MethodMap = Map.Map String (FunName, Type)
data ClassData = ClassData { _fields  :: FieldMap
                           , _methods :: MethodMap }
type ClassMap  = Map.Map String ClassData

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

        initReader = ReaderEnv { _funs      = funcMap `Map.union` library
                               , _classes   = classes
                               , _varTypes  = Map.empty
                               , _className = Nothing }
        initState  = StateEnv  { _varSupply = supp
                               , _nextLabel = 0 }

        supp = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

        (fndefs, base, der) = flip foldl' ([], [], []) sortDef topdefs
        sortDef (f, b, d) def = case def of
            x@(FnTopDef _) -> (x:f, b, d)
            x@(ClassDef {}) -> (f, x:b, d)
            x@(ClassExtDef {}) -> (f, b, x:d)

        funcMap = Map.fromList $ 
            map (\(FnTopDef (FnDef v k _ _)) -> (k, v)) fndefs
        baseClasses = Map.fromList $ 
            map (\(ClassDef name cd) -> (name, classData 0 name cd)) base
        sortedDer = sortExt der
        extClasses = flip foldl' baseClasses
            (\acc el -> uncurry Map.insert (classGetter el acc) acc) sortedDer
        classes = Map.union extClasses baseClasses
        classGetter (ClassExtDef name ext cd) classMap =
            let extData = classMap Map.! ext
                extSize = Map.size (extData^.fields)
                drvData = classData extSize name cd
                merged = ClassData 
                    { _fields = Map.union (drvData^.fields) (extData^.fields)
                    , _methods = Map.union (drvData^.methods) (extData^.methods)
                    }
            in (name, merged)
        
classData :: Int -> String -> [ClassDecl] -> ClassData
classData st cname = fst . flip foldl' (emptyClass, st)
    (\(m, i) el -> case el of
        FieldDef t f -> (over fields (Map.insert f (i, t)) m, i + 1)
        MethodDef (FnDef t name _ _) -> 
            (over methods (Map.insert name (MethodName cname name, t)) m, i))
    where
        emptyClass = ClassData { _fields = Map.empty, _methods = Map.empty }

sortExt :: [TopDef] -> [TopDef]
sortExt exts = ((\(a, _, _) -> a) . nodeFromVertex) <$> sorted
    where
        indexNodes (l, i) c@(ClassExtDef name _ _) = ((name, i):l, i + 1)
        nameMap = Map.fromList . fst $ foldl' indexNodes ([], 0) exts
        createNode c@(ClassExtDef name ext _) = 
            (c, nameMap Map.! name, maybe [] (:[]) (Map.lookup ext nameMap))
        graphNodes = createNode <$> exts
        (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges graphNodes
        sorted = reverse $ Graph.topSort graph

fields :: Lens' ClassData FieldMap
fields = lens _fields (\cd newFlds -> cd { _fields = newFlds })

methods :: Lens' ClassData MethodMap
methods = lens _methods (\cd newMeth -> cd { _methods = newMeth })

processProg :: Program -> GenState ()
processProg (Program topdefs) = forM_ topdefs processTopDef

processTopDef :: TopDef -> GenState ()
processTopDef (FnTopDef fndef) = processFnDef fndef
processTopDef (ClassExtDef i ext decls) = processTopDef (ClassDef i decls)
processTopDef (ClassDef i decls) = local (\r -> r { _className = Just i }) $
    mapM_ processFnDef (filterMethods decls [])
    where
        filterMethods [] acc = acc
        filterMethods (h:t) acc = case h of
            MethodDef fndef -> filterMethods t ((addSelf fndef):acc)
            FieldDef {} -> filterMethods t acc
        addSelf (FnDef t name args block) = 
            FnDef t name ((Arg (TClass i) "self"):args) block

processFnDef :: FnDef -> GenState ()
processFnDef fndef@(FnDef t name args block) = do
    className <- asks _className
    let name' = case className of
            Just cn -> MethodName cn name
            Nothing -> FunName name
    output (FunHead t name' args)
    let varTypesMap = typeFnDef fndef
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
    case s of
        EVar i -> output $ Assign (Var i (varType tmp)) tmp
        EArrGet e1 e2 -> do
            e1' <- processExpr e1
            e2' <- processExpr e2
            output $ ArrStore e1' e2' tmp
        EFieldGet e field -> do
            e' <- processExpr e
            let TClass cname = varType e'
            flds <- asks $ (^.fields) . flip (Map.!) cname . _classes
            let fld = fst $ flds Map.! field
            output $ ClassStore e' fld tmp
    return False
processStmt (Incr s) = processIncrStmt s (BAdd BPlus)
processStmt (Decr s) = processIncrStmt s (BAdd BMinus)
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
    output $ WhileJmp tmp l1 lEnd
    output $ Label lEnd
    return False
processStmt (SExp expr) = processExpr expr >> return False  -- ?
processStmt (For t vn expr stmt) = do
    Temp temp _ <- nextVar TInt
    processStmt (Decl TInt [Init temp (ELitInt 0)])
    let tempVar = EVar temp
        whileExpr = ERel tempVar LTH (EFieldGet expr "length")
        vnDecl = Decl t [Init vn (EArrGet expr tempVar)]
        incr   = Incr tempVar
        whileStmt = case stmt of
            BStmt (Block stmts) -> BStmt . Block $ vnDecl : stmts ++ [incr]
            stmt -> BStmt $ Block [vnDecl, stmt, incr]
    processStmt (While whileExpr whileStmt)

processIncrStmt :: Expr -> OpBin -> GenState Bool
processIncrStmt (EVar i) op = do
    output $ Binary (Var i TInt) (Var i TInt) op (CInt 1)
    return False
processIncrStmt (EArrGet e1 e2) op = do
    e1' <- processExpr e1
    e2' <- processExpr e2
    let TArray t = varType e1'
    tmp <- nextVar t
    output $ ArrLoad tmp e1' e2'
    output $ Binary tmp tmp op (CInt 1)
    output $ ArrStore e1' e2' tmp
    return False
processIncrStmt (EFieldGet e field) op = do
    e' <- processExpr e
    let t@(TClass cname) = varType e'
    tmp <- getClassField field t e'
    output $ Binary tmp tmp op (CInt 1)
    flds <- asks $ (^.fields) . flip (Map.!) cname . _classes
    let fld = fst $ flds Map.! field
    output $ ClassStore e' fld tmp
    return False

processItem :: Type -> Item -> GenState ()
processItem t (NoInit s) = output $ Assign (Var s t) defVal
    where
        defVal = case t of
            TInt -> CInt 0
            TStr -> CString ""
            TBool -> CBool False
            t@(TClass _) -> CNull t
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
    let fname' = FunName fname
    output $ (if ftype == TVoid then Call else FCall t) fname' (length elist)
    return t
processExpr (EString s) = return . CString . strip $ s
    where 
        strip = lstrip . rstrip
        lstrip = dropWhile (== '\"')
        rstrip = reverse . lstrip . reverse
processExpr (EArr t expr) = do
    expr' <- processExpr expr
    tmp   <- nextVar t
    newFs <- asks $ Map.insert arrayAlloc t . _funs
    app'  <- local (\r -> r { _funs = newFs }) $
        processExpr (EApp arrayAlloc [varToExpr expr', ELitInt 4])
    output $ Assign tmp app'
    return tmp
processExpr (EClass t@(TClass className)) = do
    newFs <- asks $ Map.insert classAlloc t . _funs
    csize <- asks $ Map.size . (^.fields) . flip (Map.!) className . _classes
    app'  <- local (\r -> r { _funs = newFs }) $
        processExpr (EApp classAlloc [ELitInt (toInteger csize)])
    tmp   <- nextVar t
    output $ Assign tmp app'
    return tmp
processExpr (EArrGet e1 e2) = do
    e1' <- processExpr e1
    e2' <- processExpr e2
    let TArray t = varType e1'
    tmp <- nextVar t
    output $ ArrLoad tmp e1' e2'
    return tmp
processExpr (EFieldGet e s) = do
    e' <- processExpr e
    tmp <- case e' of
            Var _ t@(TArray _)  -> getArraySize t e'
            Temp _ t@(TArray _) -> getArraySize t e'
            Var _ t@(TClass _)  -> getClassField s t e'
            Temp _ t@(TClass _) -> getClassField s t e'
    return tmp
processExpr (EMethod e mname exprs) = do
    e' <- processExpr e
    elist  <- forM exprs processExpr
    elist' <- forM (e':elist) constToTemp
    forM_ (reverse elist') (output . Param)
    let TClass cname = varType e'
    classes <- asks _classes
    mtds <- asks $ (^.methods) . flip (Map.!) cname . _classes
    let (mtd, ftype) = mtds Map.! mname
    t <- nextVar ftype
    output $ (if ftype == TVoid then Call else FCall t) mtd (length elist')
    return t

processExpr (ENull s) = return $ CNull (TClass s)
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
processExpr (EAnd e1 e2) = do
    lTrue  <- nextLabel
    lFalse <- nextLabel
    lEnd   <- nextLabel
    a1 <- processExpr e1
    output $ IfJmp a1 lTrue lFalse
    output $ Label lTrue
    a2 <- processExpr e2
    t  <- nextVar (varType a1)
    output $ Assign t a2
    output $ Goto lEnd
    output $ Label lFalse
    output $ Assign t (CBool False)
    output $ Goto lEnd
    output $ Label lEnd
    return t
processExpr (EOr e1 e2) = do
    lTrue  <- nextLabel
    lFalse <- nextLabel
    lEnd   <- nextLabel
    a1 <- processExpr e1
    output $ IfJmp a1 lTrue lFalse
    t  <- nextVar (varType a1)
    output $ Label lTrue
    output $ Assign t (CBool True)
    output $ Goto lEnd
    output $ Label lFalse
    a2 <- processExpr e2
    output $ Assign t a2
    output $ Goto lEnd
    output $ Label lEnd
    return t

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
            [varToExpr a1, varToExpr a2]) >>= (output . Assign t)
    return t

getArraySize :: Type -> Var -> GenState Var 
getArraySize t v = do
    tmp <- nextVar t
    output $ ArrSize tmp v
    return tmp

getClassField :: String -> Type -> Var -> GenState Var
getClassField field t@(TClass cname) v = do
    flds <- asks $ (^.fields) . flip (Map.!) cname . _classes
    let (fld, t) = flds Map.! field
    tmp  <- nextVar t
    output $ ClassLoad tmp v fld
    return tmp

varToExpr :: Var -> Expr
varToExpr v = case v of
    Var s _ -> EVar s
    Temp s _ -> EVar s
    CInt i -> ELitInt i 
    CBool b -> if b then ELitTrue else ELitFalse 
    CString s -> EString s

constToTemp :: Var -> GenState Var
constToTemp v@Var  {} = return v
constToTemp t@Temp {} = return t
constToTemp c = do
    tmp <- nextVar (varType c)
    output (Assign tmp c)
    return tmp

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
    return $ Temp ("_t_" ++ x) t

-- new GHC version fix, courtesy of haskell-chart repository
-- https://github.com/timbod7/haskell-chart/pull/197
fromInfiniteList :: [a] -> (a, [a])
fromInfiniteList []     = error "VarSupply: empty list"
fromInfiniteList (x:xs) = (x, xs)

insertIfNot :: (Ord k) => k -> v -> Map.Map k v -> Map.Map k v
insertIfNot key value map = case Map.lookup key map of
    Nothing -> Map.insert key value map
    Just _  -> map
