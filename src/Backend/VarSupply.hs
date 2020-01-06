-- VarSupplyT is heavily based on a standard implementation of SupplyT

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.VarSupply (
    MonadVarSupply(..),
    VarSupplyT,
    VarSupply,
    evalVarSupplyT,
    evalVarSupply,
    runVarSupplyT,
    runVarSupply
) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map

newtype VarSupplyT m a = VarSupplyT (StateT [String] m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

newtype VarSupply a = VarSupply (VarSupplyT Identity a)
    deriving (Functor, Applicative, Monad, MonadVarSupply)

class Monad m => MonadVarSupply m where
    nextVar   :: m String
    buildVar  :: (String -> String) -> m String
    getSupply :: m [String]
    putSupply :: [String] -> m ()

instance Monad m => MonadVarSupply (VarSupplyT m) where
    nextVar = VarSupplyT $ do
        (x, xs) <- fmap fromInfiniteList get
        put xs
        return x
    buildVar f = fmap f nextVar
    getSupply = VarSupplyT $ get
    putSupply newSupp = VarSupplyT $ put newSupp

instance MonadVarSupply m => MonadVarSupply (ReaderT r m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

instance MonadVarSupply m => MonadVarSupply (StateT s m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

instance (MonadVarSupply m, Monoid w) => MonadVarSupply (WriterT w m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

instance (MonadVarSupply m) => MonadVarSupply (ExceptT e m) where
    nextVar   = lift nextVar
    buildVar  = lift . buildVar
    getSupply = lift getSupply
    putSupply = lift . putSupply

evalVarSupplyT :: Monad m => VarSupplyT m a -> [String] -> m a
evalVarSupplyT (VarSupplyT vs) = evalStateT vs

evalVarSupply :: VarSupply a -> [String] -> a
evalVarSupply (VarSupply s) = runIdentity . evalVarSupplyT s

runVarSupplyT :: Monad m => VarSupplyT m a -> [String] -> m (a, [String])
runVarSupplyT (VarSupplyT vs) = runStateT vs

runVarSupply :: VarSupply a -> [String] -> (a, [String])
runVarSupply (VarSupply s) = runIdentity . runVarSupplyT s

-- new GHC version fix, courtesy of haskell-chart repository
-- https://github.com/timbod7/haskell-chart/pull/197
fromInfiniteList :: [a] -> (a, [a])
fromInfiniteList []     = error "VarSupply: empty list"
fromInfiniteList (x:xs) = (x, xs)
