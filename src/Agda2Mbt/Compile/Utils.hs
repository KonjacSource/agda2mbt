module Agda2Mbt.Compile.Utils where


import Control.Monad ( forM_ )
import Control.Arrow ( Arrow((***)), (&&&) )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer ( tell )
import Control.Monad.State ( put, modify )

import Data.IORef
import Data.List ( isPrefixOf, stripPrefix )
import Data.Maybe ( isJust )
import qualified Data.Map as M
import Data.String ( IsString(..) )
import Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.List as L

import GHC.Stack (HasCallStack)

import qualified Language.Haskell.Exts as Hs

import System.FilePath ( (</>) )

import Agda.Compiler.Backend hiding ( Args )
import Agda.Compiler.Common ( compileDir )

import Agda.Syntax.Common
import qualified Agda.Syntax.Concrete.Name as C
import Agda.Syntax.Internal
import Agda.Syntax.Position ( noRange )
import Agda.Syntax.Scope.Base
import Agda.Syntax.Scope.Monad ( bindVariable, freshConcreteName, isDatatypeModule )
import Agda.Syntax.TopLevelModuleName
import Agda.Syntax.Common.Pretty ( prettyShow )
import qualified Agda.Syntax.Common.Pretty as P

import Agda.TypeChecking.CheckInternal ( infer )
import Agda.TypeChecking.Constraints ( noConstraints )
import Agda.TypeChecking.Conversion ( equalTerm )
import Agda.TypeChecking.InstanceArguments ( findInstance )
import Agda.TypeChecking.Level ( isLevelType )
import Agda.TypeChecking.MetaVars ( newInstanceMeta )
import Agda.TypeChecking.Monad.SizedTypes ( isSizeType )
import Agda.TypeChecking.Pretty ( Doc, (<+>), text, PrettyTCM(..), pretty )
import Agda.TypeChecking.Records ( isRecordConstructor, getRecordOfField )
import Agda.TypeChecking.Reduce ( instantiate, reduce )
import Agda.TypeChecking.Substitute ( Subst, TelV(TelV), Apply(..) )
import Agda.TypeChecking.Telescope ( telView )

import Agda.Utils.Lens ( (<&>) )
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Singleton
import Agda.Utils.Impossible ( __IMPOSSIBLE__ )

import Agda2Mbt.Compile.Types

agda2mbtError :: (HasCallStack, MonadTCError m) => Doc -> m a
agda2mbtError msg = typeError $ CustomBackendError "agda2mbt" msg

agda2mbtErrorM :: (HasCallStack, MonadTCError m) => m Doc -> m a
agda2mbtErrorM msg = agda2mbtError =<< msg  

agda2mbtStringError :: (HasCallStack, MonadTCError m) => String -> m a
agda2mbtStringError = agda2mbtError . fromString

primModules =
  [ "Agda.Builtin"
  , "Agda.Primitive"
  , "MoonBit.Prim"
  , "MoonBit.Prelude"
  ]

moduleFileName :: Options -> TopLevelModuleName -> TCM FilePath
moduleFileName opts name = do
  outDir <- compileDir
  return $ fromMaybe outDir (optOutDir opts) </> moduleNameToFileName name "mbt"

showTCM :: PrettyTCM a => a -> C String
showTCM x = liftTCM $ show <$> prettyTCM x

setCurrentRangeQ :: QName -> C a -> C a
setCurrentRangeQ = setCurrentRange . nameBindingSite . qnameName

isInScopeUnqualified :: QName -> C Bool
isInScopeUnqualified x = lift $ do
  ys <- inverseScopeLookupName' AmbiguousAnything x <$> getScope
  return $ any (not . C.isQualified) ys


freshString :: String -> C String
freshString s = liftTCM $ do
  scope <- getScope
  ctxNames <- map (prettyShow . nameConcrete) <$> getContextNames
  let freshName = L.uncons $ filter (isFresh scope ctxNames) $ s : map (\i -> s ++ show i) [0..]
  return (maybe __IMPOSSIBLE__ fst freshName)
  where
    dummyName s = C.QName $ C.Name noRange C.NotInScope $ singleton $ C.Id s
    isFresh scope ctxNames s =
      null (scopeLookup (dummyName s) scope :: [AbstractName]) &&
      notElem s ctxNames

getInlineSymbols :: C (Set QName)
getInlineSymbols = do
  ilSetRef <- asks $ inlineSymbols . globalEnv
  liftIO $ readIORef ilSetRef


isPropSort :: Sort -> C Bool
isPropSort s = reduce s <&> \case
  Prop _ -> True
  _      -> False

-- Determine whether it is ok to erase arguments of this type,
-- even in the absence of an erasure annotation.
canErase :: Type -> C Bool
canErase a = do
  TelV tel b <- telView a
  addContext tel $ orM
    [ isErasedBaseType (unEl b)
    , isPropSort (getSort b)            -- _ : Prop
    ]

isErasedBaseType :: Term -> C Bool
isErasedBaseType x = orM
  [ isLevelType b                       -- Level
  , isJust <$> isSizeType b             -- Size
  ]
  where b = El __DUMMY_SORT__ x

withNestedType :: C a -> C a
withNestedType = local $ \e -> e { isNestedInType = True }
