module Frontend.TypeCheckerStructs where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Lens

import AST.AbsLatte
import Frontend.Exceptions
import Frontend.ReturnType

type TypeU = Type ()
data TypeM = TypeM { _type :: TypeU, _outer :: Bool }
type PosType = Maybe (Int, Int)
type ReturnTypeU = ReturnType TypeU

type FuncMap = Map.Map String TypeM

data ClDef = ClDef { _methods :: FuncMap
                   , _fields  :: Map.Map String TypeU
                   , _extends :: Maybe String }
type ClassMap = Map.Map String ClDef

data StateTuple = StateTuple { _funcs   :: FuncMap
                             , _classes :: ClassMap
                             , _types   :: Set.Set TypeU }
type StateErr = (PosType, FrontendException ())

typeM :: TypeU -> TypeM
typeM t = TypeM { _type = t, _outer = False }

methods :: Lens' ClDef FuncMap
methods = lens _methods (\cldef newMeth -> cldef { _methods = newMeth })

fields :: Lens' ClDef (Map.Map String TypeU)
fields = lens _fields (\cldef newFld -> cldef { _fields = newFld })

extends :: Lens' ClDef (Maybe String)
extends = lens _extends (\cldef newMaybe -> cldef { _extends = newMaybe })

funcs :: Lens' StateTuple FuncMap
funcs = lens _funcs (\st newFuncs -> st { _funcs = newFuncs })

classes :: Lens' StateTuple ClassMap
classes = lens _classes (\st newClasses -> st { _classes = newClasses })

types :: Lens' StateTuple (Set.Set TypeU)
types = lens _types (\st newTps -> st { _types = newTps })
