module Frontend.ConstantExpressions (
    evaluateBool,
) where

import AST.AbsLatte (Expr(..), AddOp(..), MulOp(..), RelOp(..))

data CVal = CInt Integer | CBool Bool

evaluateBool :: Expr a -> Maybe Bool
evaluateBool e = evaluate e >>= \x -> case x of
    CBool b -> Just b
    _ -> Nothing

evaluate :: Expr a -> Maybe CVal
evaluate (ELitInt _ i) = Just $ CInt i
evaluate (ELitTrue _) = Just $ CBool True
evaluate (ELitFalse _) = Just $ CBool False
evaluate (Neg _ e) = evaluate e >>= \e' -> case e' of
    CInt i -> Just $ CInt (-i)
    _ -> Nothing
evaluate (Not _ e) = evaluate e >>= \e' -> case e' of
    CBool b -> Just $ CBool (not b)
    _ -> Nothing
evaluate (EMul _ e1 op e2) = do
    e1' <- evaluate e1
    e2' <- evaluate e2
    case (e1', e2') of
        (CInt i1, CInt i2) -> Just . CInt $ mulOp op i1 i2
        _ -> Nothing
evaluate (EAdd _ e1 op e2) = do
    e1' <- evaluate e1
    e2' <- evaluate e2
    case (e1', e2') of
        (CInt i1, CInt i2) -> Just . CInt $ addOp op i1 i2
        _ -> Nothing
evaluate (ERel _ e1 op e2) = do
    e1' <- evaluate e1
    e2' <- evaluate e2
    fmap CBool $ case (e1', e2', op) of
        (CInt i1, CInt i2, LTH _)   -> Just $ i1 < i2
        (CInt i1, CInt i2, LE _)    -> Just $ i1 <= i2
        (CInt i1, CInt i2, GTH _)   -> Just $ i1 > i2
        (CInt i1, CInt i2, GE _)    -> Just $ i1 >= i2
        (CInt i1, CInt i2, EQU _)   -> Just $ i1 == i2
        (CInt i1, CInt i2, NE _)    -> Just $ i1 /= i2
        (CBool b1, CBool b2, EQU _) -> Just $ b1 == b2
        (CBool b1, CBool b2, NE _)  -> Just $ b1 /= b2
        _ -> Nothing
evaluate (EAnd _ e1 e2) = evaluateConnectors e1 e2 (&&)
evaluate (EOr _ e1 e2) = evaluateConnectors e1 e2 (||)

evaluateConnectors :: Expr a -> Expr a -> (Bool -> Bool -> Bool) -> Maybe CVal
evaluateConnectors e1 e2 fb = do
    e1' <- evaluate e1
    e2' <- evaluate e2
    case (e1', e2') of
        (CBool b1, CBool b2) -> Just . CBool $ b1 `fb` b2
        _ -> Nothing

mulOp :: MulOp a -> (Integer -> Integer -> Integer)
mulOp (Times _) = (*)
mulOp (Div _) = div
mulOp (Mod _) = mod

addOp :: AddOp a -> (Integer -> Integer -> Integer)
addOp (Plus _) = (+)
addOp (Minus _) = (-)
