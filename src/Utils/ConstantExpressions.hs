module Utils.ConstantExpressions (
    evaluateBool,
    evaluateInteger,
    CVal (..),
    Evaluable(..),
) where

import qualified AST.AbsLatte as Abs
import qualified Frontend.AST as FAst

data CVal = CInt Integer | CBool Bool deriving (Show)

class Evaluable a where
    evaluate :: a -> Maybe CVal

evaluateInteger :: (Evaluable a) => a -> Maybe Integer
evaluateInteger e = evaluate e >>= \x -> case x of
    CInt i -> Just i
    _ -> Nothing

evaluateBool :: (Evaluable a) => a -> Maybe Bool
evaluateBool e = evaluate e >>= \x -> case x of
    CBool b -> Just b
    _ -> Nothing

evaluateConnectors :: (Evaluable a) => 
    a -> a -> (Bool -> Bool -> Bool) -> Maybe CVal
evaluateConnectors e1 e2 fb = do
    e1' <- evaluate e1
    e2' <- evaluate e2
    case (e1', e2') of
        (CBool b1, CBool b2) -> Just . CBool $ b1 `fb` b2
        _ -> Nothing

instance Evaluable (Abs.Expr a) where
    evaluate (Abs.ELitInt _ i) = Just $ CInt i
    evaluate (Abs.ELitTrue _) = Just $ CBool True
    evaluate (Abs.ELitFalse _) = Just $ CBool False
    evaluate (Abs.Neg _ e) = evaluate e >>= \e' -> case e' of
        CInt i -> Just $ CInt (-i)
        _ -> Nothing
    evaluate (Abs.Not _ e) = evaluate e >>= \e' -> case e' of
        CBool b -> Just $ CBool (not b)
        _ -> Nothing
    evaluate (Abs.EMul _ e1 op e2) = do
        e1' <- evaluate e1
        e2' <- evaluate e2
        case (e1', e2') of
            (CInt i1, CInt i2) -> Just . CInt $ mulOp op i1 i2
            _ -> Nothing
        where
            mulOp (Abs.Times _) = (*)
            mulOp (Abs.Div _) = div
            mulOp (Abs.Mod _) = mod
    evaluate (Abs.EAdd _ e1 op e2) = do
        e1' <- evaluate e1
        e2' <- evaluate e2
        case (e1', e2') of
            (CInt i1, CInt i2) -> Just . CInt $ addOp op i1 i2
            _ -> Nothing
        where
            addOp (Abs.Plus _) = (+)
            addOp (Abs.Minus _) = (-)
    evaluate (Abs.ERel _ e1 op e2) = do
        e1' <- evaluate e1
        e2' <- evaluate e2
        fmap CBool $ case (e1', e2', op) of
            (CInt i1, CInt i2, Abs.LTH _)   -> Just $ i1 < i2
            (CInt i1, CInt i2, Abs.LE _)    -> Just $ i1 <= i2
            (CInt i1, CInt i2, Abs.GTH _)   -> Just $ i1 > i2
            (CInt i1, CInt i2, Abs.GE _)    -> Just $ i1 >= i2
            (CInt i1, CInt i2, Abs.EQU _)   -> Just $ i1 == i2
            (CInt i1, CInt i2, Abs.NE _)    -> Just $ i1 /= i2
            (CBool b1, CBool b2, Abs.EQU _) -> Just $ b1 == b2
            (CBool b1, CBool b2, Abs.NE _)  -> Just $ b1 /= b2
            _ -> Nothing
    evaluate (Abs.EAnd _ e1 e2) = evaluateConnectors e1 e2 (&&)
    evaluate (Abs.EOr _ e1 e2) = evaluateConnectors e1 e2 (||)
    evaluate _ = Nothing

instance Evaluable FAst.Expr where
    evaluate (FAst.ELitInt i) = Just $ CInt i
    evaluate FAst.ELitTrue = Just $ CBool True
    evaluate FAst.ELitFalse = Just $ CBool False
    evaluate (FAst.Neg e) = evaluate e >>= \e' -> case e' of
        CInt i -> Just $ CInt (-i)
        _ -> Nothing
    evaluate (FAst.Not e) = evaluate e >>= \e' -> case e' of
        CBool b -> Just $ CBool (not b)
        _ -> Nothing
    evaluate (FAst.EMul e1 op e2) = do
        e1' <- evaluate e1
        e2' <- evaluate e2
        case (e1', e2') of
            (CInt i1, CInt i2) -> Just . CInt $ mulOp op i1 i2
            _ -> Nothing
        where
            mulOp FAst.Times = (*)
            mulOp FAst.Div = div
            mulOp FAst.Mod = mod
    evaluate (FAst.EAdd e1 op e2) = do
        e1' <- evaluate e1
        e2' <- evaluate e2
        case (e1', e2') of
            (CInt i1, CInt i2) -> Just . CInt $ addOp op i1 i2
            _ -> Nothing
        where
            addOp FAst.Plus  = (+)
            addOp FAst.Minus = (-)
    evaluate (FAst.ERel e1 op e2) = do
        e1' <- evaluate e1
        e2' <- evaluate e2
        fmap CBool $ case (e1', e2', op) of
            (CInt i1, CInt i2, FAst.LTH)   -> Just $ i1 < i2
            (CInt i1, CInt i2, FAst.LE)    -> Just $ i1 <= i2
            (CInt i1, CInt i2, FAst.GTH)   -> Just $ i1 > i2
            (CInt i1, CInt i2, FAst.GE)    -> Just $ i1 >= i2
            (CInt i1, CInt i2, FAst.EQU)   -> Just $ i1 == i2
            (CInt i1, CInt i2, FAst.NE)    -> Just $ i1 /= i2
            (CBool b1, CBool b2, FAst.EQU) -> Just $ b1 == b2
            (CBool b1, CBool b2, FAst.NE)  -> Just $ b1 /= b2
            _ -> Nothing
    evaluate (FAst.EAnd e1 e2) = evaluateConnectors e1 e2 (&&)
    evaluate (FAst.EOr e1 e2) = evaluateConnectors e1 e2 (||)
    evaluate _ = Nothing
