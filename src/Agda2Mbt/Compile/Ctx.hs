module Agda2Mbt.Compile.Ctx where

import Control.Monad ( forM_ )
import Control.Arrow ( Arrow((***)), (&&&) )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer ( tell )
import Control.Monad.State ( put, modify )
import Data.Set as S
import Data.Map as M
import Data.String ( IsString(..) )

import Agda.Compiler.Backend hiding ( freshName)
import Agda.Syntax.Position ( Range )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName )
import Agda.TypeChecking.Warnings ( MonadWarning )
import Agda.Utils.Null
import Agda.Utils.Impossible
import Agda.TypeChecking.Pretty
import Agda.Syntax.Common.Pretty ( prettyShow )

import Agda2Mbt.Compile.Types

import qualified Language.MoonBit as Mbt

local' :: (Ctx -> Ctx) -> C a -> C a
local' f = local $ \env -> env { ctx = f (ctx env) }

freshName :: Name -> Set String -> C String
freshName (nameConcrete -> n) used = do
  let n' = prettyShow n 
  if S.notMember n' used then do
    pure n'
  else pure $ go n' used 1
  where
    go n used i = do
      let n' = n ++ show i
      if S.notMember n' used then
        n'
      else
        go n used (i + 1)

freshVar :: Name -> C Mbt.Name
freshVar n = freshName n =<< asks (varScope . ctx)

freshTyVar :: Name -> C Mbt.TName
freshTyVar n = freshName n =<< asks (tyVarScope . ctx)

addVar :: Name -> Mbt.Arity -> C a -> C a 
addVar n arity = local' $ \c -> c 
  { varScope = S.insert (nameIx n) (varScope c)
  , nameArity = M.insert (nameIx n) arity (nameArity c)
  }

initCtx :: Ctx
initCtx = Ctx
  { qnameArity = M.fromList 
      [ ("MoonBit.Prim.Tuple._×_"   , [2])
      , ("MoonBit.Prim.Tuple._×_×_" , [3])
      , ("Agda.Builtin.Bool.Bool" , [])
      , ("Agda.Builtin.Unit.⊤"  , [])
      ] 
  , nameArity  = M.empty 
  , qnameMap   = M.fromList 
      [ ("Agda.Builtin.Bool.Bool" , Right "Bool")
      , ("Agda.Builtin.Unit.⊤"  , Right "Unit")
      ]
  , nameMap    = M.empty 
  , varScope   = S.empty 
  , tyVarScope = S.fromList ["Bool", "Unit"]
  }