module Backend.X86.AsmGenerator (compile) where

import Control.Monad.RWS
import Control.Lens ((^.))
import Data.DList (DList, singleton, toList)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Bits(Bits, (.&.))
import Math.NumberTheory.Logarithms
import qualified Data.Map as Map

import Frontend.AST (Arg(..))
import Backend.Quadruples
import Backend.FuncDef
import Backend.AsmCommands
import Utils.StringUtils

import Globals (externFuncList)

data ReaderEnv = ReaderEnv { _argsLoc    :: Map.Map String Int
                           , _fname      :: String }
data StateEnv  = StateEnv  { _varLoc     :: Map.Map String Int
                           , _nextLoc    :: Int
                           , _strLoc     :: Map.Map String String
                           , _nextStrInd :: Int }
type WrtList   = DList AsmCommand

type GenerateM = RWS ReaderEnv WrtList StateEnv

compile :: [FuncDef] -> [String]
compile funcs = nasmHeader ++ (printAsmCommand <$> toList wrtList) ++ printRo
    where
        (s, wrtList) = execRWS (forM_ funcs processFuncDef) initReader initState
        ro = _strLoc s
        printRo = if Map.size ro == 0 then [] else rodata ro
        initReader = ReaderEnv { _argsLoc = Map.empty, _fname = "" }
        initState  = emptyState

emptyState :: StateEnv
emptyState = StateEnv { _varLoc     = Map.empty
                      , _nextLoc    = 1
                      , _strLoc     = Map.empty
                      , _nextStrInd = 0 }

clearFuncState :: StateEnv -> StateEnv
clearFuncState s = s { _varLoc = Map.empty, _nextLoc = 1 }

getVarLocFromState :: String -> GenerateM Int
getVarLocFromState s = do
    state <- get
    let locs = _varLoc state
    swap1_3 maybe return (Map.lookup s locs) $ do
        let loc     = _nextLoc state
            newLocs = Map.insert s loc locs
        put $ state { _varLoc = newLocs, _nextLoc = loc + 1 }
        return loc

getStrLocFromState :: String -> GenerateM String
getStrLocFromState s = do
    state <- get
    let locs = _strLoc state
    swap1_3 maybe return (Map.lookup s locs) $ do
        let ind      = _nextStrInd state
            namedInd = "_msgLC" ++ show ind 
            newLocs  = Map.insert s namedInd locs
        put $ state { _strLoc = newLocs, _nextStrInd = ind + 1 }
        return namedInd

nasmHeader :: [String]
nasmHeader = [ "section .text", "global main:function" ] ++ 
             (("extern " ++) <$> externFuncList) ++ 
             [""]

rodata :: Map.Map String String -> [String]
rodata = ("section .rodata" :) . map f . Map.toList
    where
        f (str, name) = indent $ name ++ " db \'" ++ str ++ "\', 0"

processFuncDef :: FuncDef -> GenerateM ()
processFuncDef fdef = do
    modify clearFuncState
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
    output (AsmLabel $ attachEnd fname)  -- TODO: optimize it!
    when (not onlyReturn && lCount > 0) $ 
        output (Add esp (Const $ addrSize lCount))
    unless onlyReturn $ output (Pop (Register EBP Lower32))
    output Ret

attachEnd :: String -> String
attachEnd s = ("." ++ s ++ "_end")

printQuadruple :: Quadruple -> GenerateM ()
printQuadruple (Binary lvar a op b) = processBinary lvar a op b
printQuadruple (Unary lvar UMinus a) = do
    aAddr <- getAddrOrValue a False
    output $ Mov eax aAddr
    output $ Neg eax
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
printQuadruple (Unary lvar UNot a) = do
    aAddr <- getAddrOrValue a False
    output $ Mov eax aAddr
    output $ Xor eax (Const 1)
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
printQuadruple (Label label) = output $ AsmLabel label
printQuadruple (Assign lvar rvar) = do
    rAddr <- getAddrOrValue rvar True
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr rAddr
printQuadruple (Goto glabel) = output $ Jmp glabel
printQuadruple (IfJmp var _ elseLabel) = do
    addr <- getAddrOrValue var True
    output $ Test addr addr
    output $ JmpMnem Equal elseLabel
printQuadruple (WhileJmp var whileLabel _) = do
    addr <- getAddrOrValue var True
    output $ Test addr addr
    output $ JmpMnem NonEq whileLabel
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
processBinary lvar a (BMul op) b = do
    case (a, b) of
        (CInt i, _) -> multiplyConstant lvar b op (fromIntegral i)
        (_, CInt i) -> multiplyConstant lvar a op (fromIntegral i)
        _ -> multiply lvar a op b
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

multiplyConstant :: Var -> Var -> OpMul -> Int -> GenerateM ()
multiplyConstant lvar _ BTimes 0 = do
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr (Const 0)
multiplyConstant lvar var BTimes 1 = printQuadruple (Assign lvar var)
multiplyConstant lvar var BTimes cst = do
    vAddr <- getAddrOrValue var False
    output $ Mov eax vAddr
    output $ if isPower2 cst 
        then Sar eax (Const $ cst `div` 2) 
        else IMul eax (Const cst)
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
multiplyConstant lvar var BDiv 1 = printQuadruple (Assign lvar var)
multiplyConstant lvar var BDiv cst = do
    let absCst = abs cst
    if isPower2 absCst && absCst <= 2147483648 
        then divideByTwo lvar var cst (cst < 0)
        else multiply lvar var BDiv (CInt $ fromIntegral cst)
multiplyConstant lvar _ BMod 1 = printQuadruple (Assign lvar (CInt 0))
multiplyConstant lvar var BMod cst = do
    let absCst = abs cst
    if isPower2 absCst && absCst <= 1073741824
        then moduloByTwo lvar var cst
        else multiply lvar var BMod (CInt $ fromIntegral cst)

-- Based on old gcc implementation
divideByTwo :: Var -> Var -> Int -> Bool -> GenerateM ()
divideByTwo lvar var 2 isNeg = do
    vAddr <- getAddrOrValue var False
    output $ Mov edx vAddr
    output $ Mov eax edx
    output $ Shr eax (Const 31)
    output $ Add eax edx
    output $ Sar eax (Const 1)
    when isNeg (output $ Neg eax)
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
divideByTwo lvar var 2147483648 _ = do
    vAddr <- getAddrOrValue var False
    output $ Mov eax vAddr
    output $ Shr eax (Const 31)
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax
divideByTwo lvar var n isNeg = do
    let nlog = integerLog2 (fromIntegral n) - 1
    vAddr <- getAddrOrValue var False
    output $ Mov edx vAddr
    output $ Mov eax edx
    output $ Sar eax (Const 31)
    output $ Shr eax (Const $ 31 - nlog)
    output $ Add eax edx
    output $ Sar eax (Const $ nlog + 1)
    when isNeg (output $ Neg eax)
    lAddr <- getAddrOrValue lvar False
    output $ Mov lAddr eax

moduloByTwo :: Var -> Var -> Int -> GenerateM ()
moduloByTwo lvar var n = do
    let nlog = integerLog2 $ fromIntegral n
    vAddr <- getAddrOrValue var False
    lAddr <- getAddrOrValue lvar False
    output $ Mov eax vAddr
    output Cdq
    output $ Shr edx (Const 31)
    output $ Add eax edx
    output $ And eax (Const $ n - 1)
    output $ Sub eax edx
    output $ Mov lAddr eax

multiply :: Var -> Var -> OpMul -> Var -> GenerateM ()
multiply lvar a op b = case op of
    BTimes -> processAddBinary lvar a IMul b
    BDiv   -> doDiv eax
    BMod   -> doDiv edx
    where
        doDiv reg = do
            aAddr <- getAddrOrValue a False
            output $ Mov eax aAddr
            output Cdq
            bAddr <- getAddrOrValue b False
            output $ IDiv bAddr
            lAddr <- getAddrOrValue lvar False
            output $ Mov lAddr reg

getAddrOrValue :: Var -> Bool -> GenerateM Memory
getAddrOrValue (Var s _) rightOperand = do
    argLoc <- asks (Map.lookup s . _argsLoc)
    addr <- addrSize <$> maybe (getVarLocFromState s) return argLoc
    let isArg = isJust argLoc
        mem = Stack (Just DWORD) (if isArg then addr else (-addr)) True
    case rightOperand of
        True -> do
            output $ Mov edx mem
            return edx
        False -> return mem
getAddrOrValue (Temp s _) rightOperand = do
    addr <- addrSize <$> getVarLocFromState s
    let mem = Stack (Just DWORD) (-addr) True
    case rightOperand of
        True -> do
            output $ Mov edx mem
            return edx
        False -> return mem
getAddrOrValue (CInt i) _ = return $ Const (fromIntegral i)
getAddrOrValue (CBool b) _ = return $ Const (if b then 1 else 0)
getAddrOrValue (CString s) _ = do
    addr <- getStrLocFromState s
    return $ Data Nothing addr False

addrSize :: Int -> Int
addrSize = (4 *)

eax :: Memory
eax = Register EAX Lower32

edx :: Memory
edx = Register EDX Lower32

esp :: Memory
esp = Register ESP Lower32

-- courtesy of https://stackoverflow.com/a/12131896
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip

-- courtesy of https://stackoverflow.com/a/58481625
isPower2 :: (Bits i, Integral i) => i -> Bool
isPower2 n = n .&. (n-1) == 0

output :: AsmCommand -> GenerateM ()
output = tell . singleton

-- outputIndented :: String -> GenerateM ()
-- outputIndented = tell . singleton . indent
