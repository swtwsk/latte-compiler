module Backend.TypeMapConstructor (typeFnDef) where

import qualified Data.Map as Map
import Data.List (foldl')

import Frontend.AST

type TypeMap = Map.Map String Type

typeFnDef :: FnDef -> TypeMap
typeFnDef (FnDef _ _ args block) =
    let argTypesMap = Map.fromList argsWithT 
        typedBlock  = typeBlock block
    in Map.union typedBlock argTypesMap
    where
        extractArg (Arg at i) = (i, at)
        argsWithT = foldr ((:) . extractArg) [] args

foldUnion :: (Ord k) => [Map.Map k v] -> Map.Map k v
foldUnion = foldl' Map.union Map.empty

typeBlock :: Block -> TypeMap
typeBlock (Block stmts) = foldUnion (typeStmt <$> stmts)

typeStmt :: Stmt -> TypeMap
typeStmt (BStmt block) = typeBlock block
typeStmt (Decl t items) = foldUnion (typeItem t <$> items)
typeStmt _ = Map.empty

typeItem :: Type -> Item -> TypeMap
typeItem t (NoInit s) = Map.singleton s t
typeItem t (Init s _) = Map.singleton s t
