module Frontend.Exceptions where

import AST.AbsLatte (Type, Expr)

data FrontendException a = NthArgument   { _index         :: Integer 
                                         , _function      :: String 
                                         , _expectedType  :: Type a
                                         , _gotType       :: Type a }
                         | ArgCount      { _function      :: String
                                         , _expectedCount :: Int
                                         , _gotCount      :: Int }
                         | NotAFunc      { _function      :: String }
                         | ExprType      { _expr :: Either String (Expr a)
                                         , _expectedType  :: Type a
                                         , _gotType       :: Type a }
                         | ExprsTypes    { _expr1 :: Either String (Expr a)
                                         , _expr2 :: Either String (Expr a)
                                         , _expectedType  :: Type a
                                         , _gotType1      :: Type a
                                         , _gotType2      :: Type a }
                         | UndeclaredVar { _varName       :: String }
                         | Redefinition  { _varName       :: String }
                         | DuplicatedArg { _function      :: String
                                         , _varName       :: String }
                         | NoReturnErr   { _function      :: String }
                         | WrongRetType  { _function      :: String
                                         , _expectedType  :: Type a
                                         , _gotType       :: Type a }
                         | DifferentTypes{ _gotType1      :: Type a
                                         , _gotType2      :: Type a }
                         | EqualityErr   { _gotType1      :: Type a
                                         , _gotType2      :: Type a }
                         | CannotAdd     { _gotType1      :: Type a
                                         , _gotType2      :: Type a }
                         | NoExtension   { _className     :: String 
                                         , _expectedExt   :: String }
                         | WrongLValue (Expr a)
                         | NoMain
                         | Unexpected
                         
instance Show (FrontendException a) where
    show e@NthArgument{} = 
        "Wrong type of " ++ show (_index e) ++ ithEnd (_index e) ++ 
        " argument in function " ++ show (_function e) ++ ". " ++
        "Expected " ++ show (_expectedType e) ++ 
        ", got " ++ show (_gotType e)
    show e@ArgCount{} =
        "Wrong number of arguments of function " ++ show (_function e) ++ 
        ". Expected " ++ show (_expectedCount e) ++ 
        ", got " ++ show (_gotCount e)
    show (NotAFunc f) = f ++ " is not a function and cannot be applied"
    show e@ExprType{} = 
        either id show (_expr e) ++ " has type " ++ show (_gotType e) ++ 
        ", but is expected to have " ++ show (_expectedType e)
    show e@ExprsTypes{} =
        "Expected expressions " ++ either id show (_expr1 e) ++ 
        " and " ++ either id show (_expr2 e) ++ " to have type " ++
        show (_expectedType e) ++ ", but they have types " ++ 
        show (_gotType1 e) ++ " and " ++ show (_gotType2 e) ++ " respectively"
    show (UndeclaredVar v) = "Undeclared variable " ++ v
    show (Redefinition v)  = "Redefinition of variable " ++ v
    show (DuplicatedArg f arg) = 
        "Duplicated argument " ++ arg ++ " in header of function " ++ f
    show (NoReturnErr f) = "No return inside function " ++ f
    show e@WrongRetType{} =
        "Expected function " ++ _function e ++ " to return a type " ++
        show (_expectedType e) ++ ", but a return inside it returns type " ++ 
        show (_gotType e)
    show (DifferentTypes t1 t2) =
        "Different return types inside a code block. One has type " ++ show t1
        ++ ", while the other " ++ show t2
    show (EqualityErr t1 t2) =
        "Cannot check equality of types " ++ show t1 ++ " and " ++ show t2
    show (CannotAdd t1 t2) = "Cannot add/concat expressions of types " ++ 
                             show t1 ++ " and " ++ show t2
    show (NoExtension cn expExt) = "Could not find base class " ++ show expExt
        ++ " for class " ++ cn
    show (WrongLValue expr) = 
        show expr ++ " is not a l-value and cannot be assigned"
    show NoMain = "Undefined reference to `int main()`"
    show Unexpected = "Unexpected error"

ithEnd :: Integer -> String
ithEnd i
    | ih == 11 || ih == 12 || ih == 13 = "th"
    | it == 1 = "st"
    | it == 2 = "nd"
    | it == 3 = "rd"
    | otherwise = "th"
    where ih = i `mod` 100
          it = i `mod` 10
