module Backend.FuncDef (
    toFuncDefs,
    FuncDef(..),
    funName,
    funType,
    funArgs,
    quads,
    locCount,
) where

import Control.Lens
import Control.Monad.State
import Control.Monad (unless)
import qualified Data.Set as Set
import qualified Data.List as List

import Frontend.AST (Type(..), Arg(..))
import Backend.Quadruples

toFuncDefs :: [Quadruple] -> [FuncDef]
toFuncDefs qs =
    let s = execState (mapM_ processQuadruple qs) blankState
        defs  = s ^. fdefs
        count = Set.size $ s ^. locals
        newDefs = case defs of
            h:t -> (h { _locCount = count }) : t
            []  -> []
    in reverse (over quads reverse <$> newDefs)
    where
        blankState = SplitState { _locals = Set.empty
                                , _fdefs  = [] }

data FuncDef = FuncDef { _funName  :: String
                       , _funType  :: Type
                       , _funArgs  :: [Arg]
                       , _locCount :: Int
                       , _quads    :: [Quadruple] } deriving (Show)

funName :: Lens' FuncDef String
funName = lens _funName (\fdef newName -> fdef { _funName = newName })

funType :: Lens' FuncDef Type
funType = lens _funType (\fdef newType -> fdef { _funType = newType })

funArgs :: Lens' FuncDef [Arg]
funArgs = lens _funArgs (\fdef newArgs -> fdef { _funArgs = newArgs })

locCount :: Lens' FuncDef Int
locCount = lens _locCount (\fdef lc -> fdef { _locCount = lc })

quads :: Lens' FuncDef [Quadruple]
quads = lens _quads (\fdef newQuads -> fdef { _quads = newQuads })

data SplitState = SplitState { _locals :: Set.Set String
                             , _fdefs  :: [FuncDef] }

locals :: Lens' SplitState (Set.Set String)
locals = lens _locals (\s newLocals -> s { _locals = newLocals })

fdefs :: Lens' SplitState [FuncDef]
fdefs = lens _fdefs (\s newFDefs -> s { _fdefs = newFDefs })

current :: Lens' SplitState FuncDef
current = lens (head . _fdefs) putCurrent
    where
        putCurrent state new =
            let (_:t) = _fdefs state in state { _fdefs = new:t }

processQuadruple :: Quadruple -> State SplitState ()
processQuadruple (FunHead tp fname args) = do
    let newDef = FuncDef { _funName = fname
                         , _funType = tp 
                         , _funArgs = args
                         , _locCount = 0
                         , _quads = [] }
    st <- get
    let defs    = st ^. fdefs
        count   = Set.size $ st ^. locals
        newDefs = case defs of
            h:t -> newDef : (h { _locCount = count }) : t
            []  -> [newDef]
    put SplitState { _locals = Set.empty, _fdefs = newDefs }
processQuadruple q@(Binary left _ _ _) = processAssigningQuadruple q left
processQuadruple q@(Unary left _ _)    = processAssigningQuadruple q left
processQuadruple q@(Assign left _)     = processAssigningQuadruple q left
processQuadruple q@(FCall left _ _)    = processAssigningQuadruple q left
processQuadruple q                     = modify (over (current . quads) (q:))

processAssigningQuadruple :: Quadruple -> Var -> State SplitState ()
processAssigningQuadruple q var = do
    ls <- gets (^. locals)
    as <- gets (^. current . funArgs)
    let lVar = case var of
            Var s _ -> if isArg s as then Nothing else Just $ "%v_" ++ s
            Temp s _ -> Just $ "%t_" ++ s
            _ -> Nothing
    maybe (return ()) (\x -> modify $ over locals (x `Set.insert`)) lVar
    modify (over (current . quads) (q:))
    where
        isArg v l = List.elem v $ (\(Arg _ i) -> i) <$> l
