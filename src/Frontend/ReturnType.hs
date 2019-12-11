module Frontend.ReturnType where

data ReturnType a = NoReturn | ConstantReturn a | Return a

instance Functor ReturnType where
    fmap f rtype = case rtype of
        NoReturn -> NoReturn
        ConstantReturn t -> ConstantReturn (f t)
        Return t -> Return (f t)

instance Show a => Show (ReturnType a) where
    show NoReturn = "NoRet"
    show (ConstantReturn t) = "Const " ++ show t
    show (Return t) = "Ret " ++ show t

returnType :: b -> (a -> b) -> (a -> b) -> ReturnType a -> b
returnType nf cf rf retType = case retType of
    NoReturn -> nf
    ConstantReturn r -> cf r
    Return r -> rf r