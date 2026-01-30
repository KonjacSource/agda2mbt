module Agda2Mbt.Compile.Var where

import Control.Arrow ( (&&&) )
import Control.Monad ( unless )
import Control.Monad.Reader.Class ( asks )
import Data.Map as M 
import Data.Set as S

import Agda.Syntax.Common
import Agda.Syntax.Internal ( unDom )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Abstract.Name ( nameConcrete )
import Agda.TypeChecking.Pretty ( text )
import Agda.TypeChecking.Monad.Base ( ContextEntry(..) )
import Agda.TypeChecking.Monad.Context ( lookupBV )
import Agda.Utils.Monad ( whenM )

import Agda2Mbt.Compile.Types
import Agda2Mbt.Compile.Utils

import qualified Language.MoonBit as Mbt

-- | Compile a variable.
compileDBVar :: Nat -> C (Mbt.Name, Mbt.Arity)
compileDBVar x = do
  n <- nameIx . ceName <$> lookupBV x
  arityMap <- asks (nameArity . ctx)
  nameMap <- asks (nameMap . ctx)
  case M.lookup n nameMap of 
    Nothing -> agda2mbtError =<< (text "Unknown variable:" <> text n)
    Just (Right _) -> agda2mbtError =<< (text "Expected term variable, found type variable:" <> text n)
    Just (Left name) -> do
      arity <- case M.lookup n arityMap of
        Just a  -> return a
        Nothing -> agda2mbtError =<< (text "Unknown variable arity for:" <> text n)
      return (name, arity)

compileDBTyVar :: Nat -> C (Mbt.TName, Mbt.Arity)
compileDBTyVar x = do 
  n <- nameIx . ceName <$> lookupBV x
  arityMap <- asks (nameArity . ctx)
  nameMap <- asks (nameMap . ctx)
  case M.lookup n nameMap of 
    Nothing -> agda2mbtError =<< (text "Unknown type variable:" <> text n)
    Just (Left _) -> agda2mbtError =<< (text "Expected type variable, found term variable:" <> text n)
    Just (Right name) -> do
      arity <- case M.lookup n arityMap of
        Just a  -> return a
        Nothing -> agda2mbtError =<< (text "Unknown type variable arity for:" <> text n)
      return (name, arity)

