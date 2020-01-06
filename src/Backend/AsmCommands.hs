module Backend.AsmCommands (
    AsmCommand(..),
    Memory(..),
    RegBitPart(..),
    Register(..),
    RelMnemonic(..),
    DataSize(..),
    printAsmCommand
) where

import qualified Data.Map as Map
import Utils.StringUtils

printAsmCommand :: AsmCommand -> String
printAsmCommand cmd = case cmd of
    al@(AsmLabel _) -> show al
    c -> indent . show $ c

data AsmCommand = Mov Memory Memory
                | Add Memory Memory
                | Sub Memory Memory
                | IMul Memory Memory
                | IDiv Memory
                | Test Memory Memory
                | Cmp Memory Memory
                | Xor Memory Memory
                | And Memory Memory
                | Or Memory Memory
                | Sar Memory Memory
                | Shr Memory Memory
                | Sal Memory Memory
                | AsmLabel String
                | Push Memory
                | Pop Memory
                | Neg Memory
                | AsmCall String
                | Jmp String
                | JmpMnem RelMnemonic String
                | Set RelMnemonic Memory
                | Cdq
                | Ret

type StackOffset = Int
type IsValue   = Bool
data Memory = Register Register RegBitPart 
            | Stack (Maybe DataSize) StackOffset IsValue
            | Data (Maybe DataSize) String IsValue
            | Const Int

data RegBitPart = Lower32 | Lower16 | Lower8 deriving (Eq, Ord)
data Register = EAX | ECX | EDX | EBP | ESP deriving (Eq, Ord)
data RelMnemonic = Lower | Greater | Equal | LowerEq | GreaterEq | NonEq
data DataSize = DWORD

instance Show AsmCommand where
    show (Mov m1 m2)   = showTwoArg "mov" m1 m2
    show (Add m1 m2)   = showTwoArg "add" m1 m2
    show (Sub m1 m2)   = showTwoArg "sub" m1 m2
    show (IMul m1 m2)  = showTwoArg "imul" m1 m2
    show (IDiv m)      = showOneArg "idiv" m
    show (Test m1 m2)  = showTwoArg "test" m1 m2
    show (Cmp m1 m2)   = showTwoArg "cmp" m1 m2
    show (Xor m1 m2)   = showTwoArg "xor" m1 m2
    show (And m1 m2)   = showTwoArg "and" m1 m2
    show (Or m1 m2)    = showTwoArg "or" m1 m2
    show (Sar m1 m2)   = showTwoArg "sar" m1 m2
    show (Shr m1 m2)   = showTwoArg "shr" m1 m2
    show (Sal m1 m2)   = showTwoArg "sal" m1 m2
    show (AsmLabel s)  = s ++ ":"
    show (Push m)      = showOneArg "push" m
    show (Pop m)       = showOneArg "pop" m
    show (Neg m)       = showOneArg "neg" m
    show (AsmCall s)   = "call " ++ s
    show (Jmp s)       = "jmp " ++ s
    show (JmpMnem r s) = "j" ++ show r ++ " " ++ s
    show (Set rel m)   = "set" ++ show rel ++ " " ++ show m
    show Cdq           = "cdq"
    show Ret           = "ret"

instance Show Memory where
    show (Register reg bitPart) = maybe "!NoReg!" id $ getRegBitName reg bitPart
    show (Stack dataSize offset isVal) = maybe "" ((++ " ") . show) dataSize ++ 
        valGetter ("ebp" ++ (if offset < 0 then " - " else " + ") ++ 
        (show $ abs offset))
        where
            valGetter = if isVal then getValue else id
    show (Data dataSize dataName isVal) = maybe "" show dataSize ++ 
        valGetter dataName
        where
            valGetter = if isVal then getValue else id
    show (Const i) = show i

instance Show RelMnemonic where
    show mnemonic = case mnemonic of
        Lower     -> "l"
        Greater   -> "g"
        Equal     -> "e"
        LowerEq   -> "le"
        GreaterEq -> "ge"
        NonEq     -> "ne"

instance Show DataSize where
    show DWORD = "DWORD"

regBitPartMap :: Map.Map Register (Map.Map RegBitPart String)
regBitPartMap = Map.fromList 
  [ (EAX, Map.fromList [ (Lower32, "eax"), (Lower16, "ax"), (Lower8, "al")  ])
  , (ECX, Map.fromList [ (Lower32, "ecx"), (Lower16, "cx"), (Lower8, "cl")  ])
  , (EDX, Map.fromList [ (Lower32, "edx"), (Lower16, "dx"), (Lower8, "dl")  ])
  , (EBP, Map.fromList [ (Lower32, "ebp"), (Lower16, "bp"), (Lower8, "bpl") ])
  , (ESP, Map.fromList [ (Lower32, "esp"), (Lower16, "sp"), (Lower8, "spl") ]) ]

getRegBitName :: Register -> RegBitPart -> Maybe String
getRegBitName reg bitPart = do
    regMap  <- Map.lookup reg regBitPartMap
    Map.lookup bitPart regMap

getValue :: String -> String
getValue s = "[" ++ s ++ "]"

showTwoArg :: String -> Memory -> Memory -> String
showTwoArg s m1 m2 = s ++ " " ++ show m1 ++ ", " ++ show m2

showOneArg :: String -> Memory -> String
showOneArg s m = s ++ " " ++ show m
