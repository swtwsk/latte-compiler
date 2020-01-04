module Backend.X86.AsmGenerator (compile) where

import Control.Monad.RWS
import Control.Lens ((^.))
import Data.DList (DList, singleton, toList)
import Data.List (foldl')
import Data.Maybe (isJust)
import qualified Data.Map as Map

import Frontend.AST (Arg(..))
import Backend.Quadruples
import Backend.FuncDef
import Backend.AsmCommands
import Utils.StringUtils

data ReaderEnv = ReaderEnv { _argsLoc :: Map.Map String Int
                           , _fname   :: String }
data StateEnv  = StateEnv  { _varLoc  :: Map.Map String Int,
                             _nextLoc :: Int }
type WrtList   = DList AsmCommand

type GenerateM = RWS ReaderEnv WrtList StateEnv

compile :: [FuncDef] -> [String]
compile funcs = nasmHeader ++ (printAsmCommand <$> toList wrtList)
    where
        (_, wrtList) = evalRWS (forM_ funcs processFuncDef) initReader initState
        initReader = ReaderEnv { _argsLoc = Map.empty, _fname = "" }
        initState  = emptyState

emptyState :: StateEnv
emptyState = StateEnv { _varLoc = Map.empty, _nextLoc = 1 }

getVarLocFromState :: String -> GenerateM Int
getVarLocFromState s = do
    locs <- gets _varLoc
    swap1_3 maybe return (Map.lookup s locs) $ do
        loc <- gets _nextLoc
        let newLocs = Map.insert s loc locs
        put $ StateEnv { _varLoc = newLocs, _nextLoc = loc + 1 }
        return loc

nasmHeader :: [String]
nasmHeader = [ "section .text"
             , "global main:function"
             --, "global _start:function"
             , "" ]

processFuncDef :: FuncDef -> GenerateM ()
processFuncDef fdef = do
    put emptyState  -- clear up the state
    printProlog (fdef^.funName) (fdef^.locCount) onlyReturn
    forM_ (fdef^.quads) (local (const newReader) . printQuadruple)
    printEpilog (fdef^.funName) (fdef^.locCount) onlyReturn
    where
        onlyReturn = case fdef^.quads of
            [Return Nothing] -> True
            _ -> False
        newReader = ReaderEnv { _argsLoc = argsMap, _fname = fdef^.funName }
        argsMap = Map.fromList . snd $ foldl' foldFun (2, []) (fdef^.funArgs)
            where
                foldFun (i, l) (Arg _ s) = (i + 1, (s,i):l)

printProlog :: String -> Int -> Bool -> GenerateM ()
printProlog fname lCount onlyReturn = do
    -- when (fname == "main") (output $ "_start:")
    output $ AsmLabel fname
    unless onlyReturn $ output (Push (Register EBP Lower32))
    unless onlyReturn $ 
        output (Mov (Register EBP Lower32) esp)
    when (not onlyReturn && lCount > 0) $
        output (Sub esp (Const $ addrSize lCount))

printEpilog :: String -> Int -> Bool -> GenerateM ()
printEpilog fname lCount onlyReturn = do
    unless onlyReturn $ output (AsmLabel $ attachEnd fname)
    when (not onlyReturn && lCount > 0) $ 
        output (Add esp (Const $ addrSize lCount))
    unless onlyReturn $ output (Pop (Register EBP Lower32))
    output Ret

attachEnd :: String -> String
attachEnd s = ("." ++ s ++ "_end")

printQuadruple :: Quadruple -> GenerateM ()
printQuadruple (Binary lvar a op b) = processBinary lvar a op b
printQuadruple (Unary lvar op a) = do
    aAddr <- getAddrOrValue a False
    output $ Mov eax aAddr
    output $ op' eax
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
    where
        op' = case op of
            UMinus -> Neg
            UNot   -> Not
printQuadruple (Label label) = output $ AsmLabel label
printQuadruple (Assign lvar rvar) = do
    rAddr <- getAddrOrValue rvar True
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr rAddr
printQuadruple (Goto glabel) = output $ Jmp glabel
printQuadruple (IfJmp var ifLabel elseLabel) = do
    addr <- getAddrOrValue var True
    output $ Test addr addr
    output $ JmpMnem Equal elseLabel
printQuadruple (Call flabel i) = do
    output $ AsmCall flabel
    when (i > 0) $ output (Add esp (Const $ addrSize i))
printQuadruple (FCall lvar flabel i) = do
    output $ AsmCall flabel
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
    when (i > 0) $ output (Add esp (Const $ addrSize i))
printQuadruple (Param var) = do
    addr <- getAddrOrValue var False
    output $ Mov eax addr
    output $ Push eax
printQuadruple (Return var) = do
    case var of
        Just v -> do
            addr <- getAddrOrValue v False
            output $ Mov eax addr
        Nothing -> return ()
    fname <- asks _fname
    output $ Jmp (attachEnd fname)

processBinary :: Var -> Var -> OpBin -> Var -> GenerateM ()
processBinary lvar a (BAdd op) b = processAddBinary lvar a op' b
    where
        op' = case op of
            BPlus -> Add
            BMinus -> Sub
processBinary lvar a (BMul op) b = undefined
processBinary lvar a (BRel op) b = do
    let ecx = Register ECX Lower32
    output $ Xor eax eax
    aAddr <- getAddrOrValue a False
    output $ Mov ecx aAddr
    bAddr <- getAddrOrValue b False
    output $ Cmp ecx bAddr
    output $ Set setMnemo (Register EAX Lower8)
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
    where
        setMnemo = case op of
            BLTH -> Lower
            BGTH -> Greater
            BEQU -> Equal
            BLE  -> LowerEq
            BGE  -> GreaterEq
            BNE  -> NonEq
processBinary lvar a (BLog op) b = processAddBinary lvar a op' b
    where
        op' = case op of
            BAnd -> And
            BOr  -> Or

type BinConst = Memory -> Memory -> AsmCommand
processAddBinary :: Var -> Var -> BinConst -> Var -> GenerateM ()
processAddBinary lvar a op b = do
    aAddr <- getAddrOrValue a False
    output $ Mov eax aAddr
    bAddr <- getAddrOrValue b False
    output $ op eax bAddr
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax

getAddrOrValue :: Var -> Bool -> GenerateM Memory
getAddrOrValue (Var s) rightOperand = do
    argLoc <- asks (Map.lookup s . _argsLoc)
    addr <- addrSize <$> maybe (getVarLocFromState s) return argLoc
    let isArg = isJust argLoc
        mem = Stack (Just DWORD) (if isArg then addr else (-addr))
    case rightOperand of
        True -> do
            let edx = Register EDX Lower32
            output $ Mov edx mem
            return edx
        False -> return mem
getAddrOrValue (Temp s) rightOperand = do
    addr <- addrSize <$> getVarLocFromState s
    let mem = Stack (Just DWORD) (-addr)
    case rightOperand of
        True -> do
            let edx = Register EDX Lower32
            output $ Mov edx mem
            return edx
        False -> return mem
getAddrOrValue (CInt i) _ = return $ Const (fromIntegral i)
getAddrOrValue (CBool b) _ = return $ Const (if b then 1 else 0)
getAddrOrValue (CString s) _ = undefined

addrSize :: Int -> Int
addrSize = (4 *)

eax :: Memory
eax = Register EAX Lower32

esp :: Memory
esp = Register ESP Lower32

-- courtesy of https://stackoverflow.com/a/12131896
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip

output :: AsmCommand -> GenerateM ()
output = tell . singleton

-- outputIndented :: String -> GenerateM ()
-- outputIndented = tell . singleton . indent
