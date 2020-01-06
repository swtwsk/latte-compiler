module Globals where

externFuncList :: [String]
externFuncList = [ printIntName
                 , printStringName
                 , errorName
                 , readIntName
                 , readStringName
                 , concatStringName ]

printIntName :: String
printIntName = "printInt"

printStringName :: String
printStringName = "printString"

errorName :: String
errorName = "error"

readIntName :: String
readIntName = "readInt"

readStringName :: String
readStringName = "readString"

concatStringName :: String
concatStringName = "__concatString"
