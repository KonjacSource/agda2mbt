module Agda2Mbt.Compile.Name where

import Control.Arrow ( (>>>) )
import Control.Applicative ( (<|>) )
import Control.Monad
import Control.Monad.Except ( catchError )
import Control.Monad.Reader

import Data.Functor ( (<&>) )
import Data.Bifunctor ( bimap )
import Data.List ( intercalate, isPrefixOf, stripPrefix )
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import Agda.Compiler.Backend hiding ( topLevelModuleName )
import Agda.Compiler.Common ( topLevelModuleName )

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common hiding (Rewrite)
import Agda.Syntax.Internal
import Agda.Syntax.Position
import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Scope.Base ( inverseScopeLookupName, amodName )
import Agda.Syntax.Scope.Monad ( resolveName, isDatatypeModule )
import Agda.Syntax.TopLevelModuleName
import Agda.Syntax.Common.Pretty ( prettyShow )
import qualified Agda.Syntax.Common.Pretty as P

import Agda.TypeChecking.Datatypes ( isDataOrRecordType )
import Agda.TypeChecking.Pretty 
import Agda.TypeChecking.Records ( isRecordConstructor )
import Agda.TypeChecking.Warnings ( warning )

import qualified Agda.Utils.List1 as List1
import Agda.Utils.Maybe
import Agda.Utils.Monad ( orM, whenM )

import Agda2Mbt.Compile.Types
import Agda2Mbt.Compile.Utils

import qualified Language.MoonBit as Mbt

lookupQName :: QName -> C (Either (Mbt.Name, Mbt.Arity) (Mbt.TName, Mbt.Arity))
lookupQName qn = do
  qnMap <- asks (qnameMap . ctx)
  qnArt <- asks (qnameArity . ctx)
  case (Map.lookup (qnameIx qn) qnMap, Map.lookup (qnameIx qn) qnArt) of
    (Just (Left name), Just arity) -> pure $ Left (name, arity) 
    (Just (Right name), Just arity) -> pure $ Right (name, arity)
    _ -> agda2mbtError =<< (text "Unknown QName:" <+> prettyTCM qn)

-- lookupName :: Name -> C (Either Mbt.Name Mbt.TName)
-- lookupName name = do
--   nameMap <- asks (nameMap . ctx)
--   case Map.lookup (nameIx name) nameMap of
--     Just n -> pure n
--     Nothing -> do 
--       agda2mbtError =<< (text "Unknown Name:" <+> pretty name)

-- compileNameVar :: Name -> C Mbt.Name
-- compileNameVar name = do
--   n <- lookupName name
--   case n of
--     Left v -> return v
--     Right _ -> agda2mbtError =<< (text "Expected variable but got type variable for:" <+> pretty name)

-- compileNameTy :: Name -> C Mbt.TName
-- compileNameTy name = do
--   n <- lookupName name
--   case n of
--     Right v -> return v
--     Left _ -> agda2mbtError =<< (text "Expected type variable but got variable for:" <+> pretty name)

compileQNameVar :: QName -> C (Mbt.Name, Mbt.Arity)
compileQNameVar qn = do
  name <- lookupQName qn
  case name of
    Left n -> return n
    Right _ -> agda2mbtError =<< (text "Expected variable but got type variable for:" <+> prettyTCM qn)

compileQNameTy :: QName -> C (Mbt.TName, Mbt.Arity)
compileQNameTy qn = do
    name <- lookupQName qn
    case name of
      Right n -> return n
      Left _ -> agda2mbtError =<< (text "Expected type variable but got variable for:" <+> prettyTCM qn)

isWhereFunction :: QName -> C Bool
isWhereFunction f = do
  whereMods <- asks whereModules
  return $ any (qnameModule f `isLeChildModuleOf`) whereMods

