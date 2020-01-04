module Backend.X86.AsmGenerator (compile) where

import Control.Monad.RWS
import Control.Lens ((^.))
import Data.DList (DList, singleton, toList)
import Data.List (foldl')
import qualified Data.Map as Map

import Frontend.AST (Arg(..))
import Backend.Quadruples
import Backend.FuncDef
import Utils.StringUtils

data ReaderEnv = ReaderEnv { _argsLoc :: Map.Map String Int
                           , _fname   :: String }
type StateEnv  = Map.Map String Int
type WrtList   = DList String

type GenerateM = RWS ReaderEnv WrtList StateEnv

compile :: [FuncDef] -> [String]
compile funcs = toList wrtList
    where
        (_, wrtList) = evalRWS (forM_ funcs processFuncDef) initReader initState
        initReader = ReaderEnv { _argsLoc = Map.empty, _fname = "" }
        initState  = Map.empty

processFuncDef :: FuncDef -> GenerateM ()
processFuncDef fdef = do
    put Map.empty  -- clear up the state
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
attachEnd = (++ "_end")

printQuadruple :: Quadruple -> GenerateM ()
printQuadruple (Binary lvar a op b) = do
    aAddr <- getAddrOrValue a False
    outputIndented $ "mov eax, " ++ aAddr
    bAddr <- getAddrOrValue b True
    outputIndented $ op' ++ " eax, " ++ bAddr
    lAddr <- getAddrOrValue lvar False
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
    aAddr <- getAddrOrValue a False
    outputIndented $ "mov eax, " ++ aAddr
    outputIndented $ op' ++ " eax"
    lAddr <- getAddrOrValue lvar False
    outputIndented $ "mov " ++ lAddr ++ ", eax"
    where
        op' = case op of
            UMinus -> "neg"
            UNot   -> "not"
printQuadruple (Label label) = output label
printQuadruple (Assign lvar rvar) = do
    rAddr <- getAddrOrValue rvar True
    lAddr <- getAddrOrValue lvar False
    outputIndented $ "mov " ++ lAddr ++ ", " ++ rAddr
printQuadruple (Goto glabel) = outputIndented $ "jmp " ++ glabel
printQuadruple (IfJmp var ifLabel elseLabel) = undefined
printQuadruple (Call flabel i) = do
    outputIndented $ "call " ++ flabel
    when (i > 0) $ outputIndented ("add esp, " ++ show (addrSize i))
printQuadruple (FCall lvar flabel i) = do
    outputIndented $ "call " ++ flabel
    lAddr <- getAddrOrValue lvar False
    outputIndented $ "mov " ++ lAddr ++ ", eax"
    when (i > 0) $ outputIndented ("add esp, " ++ show (addrSize i))
printQuadruple (Param var) = do
    addr <- getAddrOrValue var False
    outputIndented $ "mov eax, " ++ addr
    outputIndented $ "push eax"
printQuadruple (Return var) = do
    case var of
        Just v -> do
            addr <- getAddrOrValue v False
            outputIndented $ "mov eax, " ++ addr
        Nothing -> return ()
    fname <- asks _fname
    outputIndented $ "jmp " ++ attachEnd fname

getAddrOrValue :: Var -> Bool -> GenerateM String
getAddrOrValue (Var s) rightOperand = undefined
getAddrOrValue (Temp s) rightOperand = undefined
getAddrOrValue (CInt i) _ = return $ show i
getAddrOrValue (CBool b) _ = return $ if b then "1" else "0"
getAddrOrValue (CString s) _ = undefined

addrSize :: Int -> Int
addrSize = (4 *)

output :: String -> GenerateM ()
output x = tell $ singleton x

outputIndented :: String -> GenerateM ()
outputIndented = tell . singleton . indent
