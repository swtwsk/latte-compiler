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
import Utils.StringUtils

data ReaderEnv = ReaderEnv { _argsLoc :: Map.Map String Int
                           , _fname   :: String }
data StateEnv  = StateEnv  { _varLoc  :: Map.Map String Int,
                             _nextLoc :: Int }
type WrtList   = DList String

type GenerateM = RWS ReaderEnv WrtList StateEnv

compile :: [FuncDef] -> [String]
compile funcs = nasmHeader ++ toList wrtList
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
    output $ fname ++ ":"
    unless onlyReturn $ outputIndented "push ebp"
    unless onlyReturn $ outputIndented "mov ebp, esp"
    when (not onlyReturn && lCount > 0) $ 
        outputIndented ("sub esp, " ++ show (addrSize lCount))

printEpilog :: String -> Int -> Bool -> GenerateM ()
printEpilog fname lCount onlyReturn = do
    unless onlyReturn $ output $ attachEnd fname ++ ":"
    when (not onlyReturn && lCount > 0) $ 
        outputIndented ("add esp, " ++ show (addrSize lCount))
    unless onlyReturn $ outputIndented "pop ebp"
    outputIndented "ret"

attachEnd :: String -> String
attachEnd s = ("." ++ s ++ "_end")

printQuadruple :: Quadruple -> GenerateM ()
printQuadruple (Binary lvar a op b) = do
    aAddr <- getAddrOrValue a
    outputIndented $ "mov eax, " ++ aAddr
    bAddr <- getAddrOrValue b
    outputIndented $ op' ++ " eax, " ++ bAddr
    lAddr <- getAddrOrValue lvar
    outputIndented $ "mov " ++ lAddr ++ ", eax"
    where
        op' = case op of
            BPlus -> "add"
            BMinus -> "sub"
            BTimes -> undefined
            BDiv -> undefined
            BMod -> undefined
            BAnd -> "and"
            BOr -> "or"
            BLTH -> undefined
            BLE -> undefined
            BGTH -> undefined
            BGE -> undefined
            BEQU -> undefined
            BNE -> undefined
printQuadruple (Unary lvar op a) = do
    aAddr <- getAddrOrValue a
    outputIndented $ "mov eax, " ++ aAddr
    outputIndented $ op' ++ " eax"
    lAddr <- getAddrOrValue lvar
    outputIndented $ "mov " ++ lAddr ++ ", eax"
    where
        op' = case op of
            UMinus -> "neg"
            UNot   -> "not"
printQuadruple (Label label) = output $ label ++ ":"
printQuadruple (Assign lvar rvar) = do
    rAddr <- getAddrOrValue rvar
    lAddr <- getAddrOrValue lvar
    outputIndented $ "mov " ++ lAddr ++ ", " ++ rAddr
printQuadruple (Goto glabel) = outputIndented $ "jmp " ++ glabel
printQuadruple (IfJmp var ifLabel elseLabel) = do
    addr <- getAddrOrValue var
    outputIndented $ "test " ++ addr ++ ", " ++ addr
    outputIndented $ "je " ++ ifLabel
    outputIndented $ "jmp " ++ elseLabel  -- TODO: optimize it
printQuadruple (Call flabel i) = do
    outputIndented $ "call " ++ flabel
    when (i > 0) $ outputIndented ("add esp, " ++ show (addrSize i))
printQuadruple (FCall lvar flabel i) = do
    outputIndented $ "call " ++ flabel
    lAddr <- getAddrOrValue lvar
    outputIndented $ "mov " ++ lAddr ++ ", eax"
    when (i > 0) $ outputIndented ("add esp, " ++ show (addrSize i))
printQuadruple (Param var) = do
    addr <- getAddrOrValue var
    outputIndented $ "mov eax, " ++ addr
    outputIndented $ "push eax"
printQuadruple (Return var) = do
    case var of
        Just v -> do
            addr <- getAddrOrValue v
            outputIndented $ "mov eax, " ++ addr
        Nothing -> return ()
    fname <- asks _fname
    outputIndented $ "jmp " ++ attachEnd fname

getAddrOrValue :: Var -> GenerateM String
getAddrOrValue (Var s) = do
    argLoc <- asks (Map.lookup s . _argsLoc)
    addr <- addrSize <$> maybe (getVarLocFromState s) return argLoc
    let isArg = isJust argLoc
    return $ "DWORD [" ++ offset isArg addr ++ "]"
    where
        offset :: Bool -> Int -> String
        offset isArg i = (if isArg then "ebp + " else "ebp - ") ++ show i
getAddrOrValue (Temp s) = do
    addr <- addrSize <$> getVarLocFromState s
    return $ "DWORD [ebp - " ++ show addr ++ "]"
getAddrOrValue (CInt i) = return $ show i
getAddrOrValue (CBool b) = return $ if b then "1" else "0"
getAddrOrValue (CString s) = undefined

addrSize :: Int -> Int
addrSize = (4 *)

-- courtesy of https://stackoverflow.com/a/12131896
swap1_3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
swap1_3 = (flip .) . flip

output :: String -> GenerateM ()
output x = tell $ singleton x

outputIndented :: String -> GenerateM ()
outputIndented = tell . singleton . indent
