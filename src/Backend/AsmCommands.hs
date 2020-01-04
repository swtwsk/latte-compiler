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
                | Test Memory Memory
                | Cmp Memory Memory
                | Xor Memory Memory
                | And Memory Memory
                | Or Memory Memory
                | AsmLabel String
                | Push Memory
                | Pop Memory
                | Neg Memory
                | Not Memory
                | AsmCall String
                | Jmp String
                | JmpMnem RelMnemonic String
                | Set RelMnemonic Memory
                | Ret

type StackOffset = Int
data Memory = Register Register RegBitPart 
            | Stack (Maybe DataSize) StackOffset 
            | Data (Maybe DataSize) String 
            | Const Int

data RegBitPart = Lower32 | Lower16 | Lower8 deriving (Eq, Ord)
data Register = EAX | ECX | EDX | EBP | ESP deriving (Eq, Ord)
data RelMnemonic = Lower | Greater | Equal | LowerEq | GreaterEq | NonEq
data DataSize = DWORD

instance Show AsmCommand where
    show (Mov m1 m2)   = showTwoArg "mov" m1 m2
    show (Add m1 m2)   = showTwoArg "add" m1 m2
    show (Sub m1 m2)   = showTwoArg "sub" m1 m2
    show (Test m1 m2)  = showTwoArg "test" m1 m2
    show (Cmp m1 m2)   = showTwoArg "cmp" m1 m2
    show (Xor m1 m2)   = showTwoArg "xor" m1 m2
    show (And m1 m2)   = showTwoArg "and" m1 m2
    show (Or m1 m2)    = showTwoArg "or" m1 m2
    show (AsmLabel s)  = s ++ ":"
    show (Push m)      = showOneArg "push" m
    show (Pop m)       = showOneArg "pop" m
    show (Neg m)       = showOneArg "neg" m
    show (Not m)       = showOneArg "not" m
    show (AsmCall s)   = "call " ++ s
    show (Jmp s)       = "jmp " ++ s
    show (JmpMnem r s) = "j" ++ show r ++ " " ++ s
    show (Set rel m)   = "set" ++ show rel ++ " " ++ show m
    show Ret           = "ret"

instance Show Memory where
    show (Register reg bitPart) = maybe "!NoReg!" id $ getRegBitName reg bitPart
    show (Stack dataSize offset) = maybe "" ((++ " ") . show) dataSize ++ 
        "[ebp " ++ (if offset < 0 then "- " else "+ ") ++ 
        (show $ abs offset) ++ "]"
    show (Data dataSize dataName) = maybe "" show dataSize ++ 
        "[" ++ dataName ++ "]"
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

showTwoArg :: String -> Memory -> Memory -> String
showTwoArg s m1 m2 = s ++ " " ++ show m1 ++ ", " ++ show m2

showOneArg :: String -> Memory -> String
showOneArg s m = s ++ " " ++ show m
