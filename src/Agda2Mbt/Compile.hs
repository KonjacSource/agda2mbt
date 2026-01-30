module Agda2Mbt.Compile where

import Prelude hiding (null)

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.CPS ( evalRWST )
import Control.Monad.State ( gets, liftIO )
import Control.Arrow ((>>>))
import Data.Functor
import Data.IORef
import Data.List ( isPrefixOf, group, sort )

import qualified Data.Map as M
import qualified Data.Set as S

import Agda.Compiler.Backend
import Agda.Compiler.Common ( curIF )
import Agda.Utils.FileName ( isNewerThan )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Monad.Signature ( isInlineFun )
import Agda.Utils.Impossible
import Agda.Utils.List
import Agda.Utils.Null
import Agda.Utils.Monad ( whenM, anyM, when, unless )

import Agda2Mbt.Compile.Types 
import Agda2Mbt.Config
import Agda2Mbt.Compile.Utils
import Agda2Mbt.Pragma
import Agda2Mbt.Compile.Function 
import Agda2Mbt.Compile.Data

import qualified Language.MoonBit as Mbt
import Agda2Mbt.Compile.Ctx

globalSetup :: Options -> TCM GlobalEnv
globalSetup opts = do
  opts <- checkConfig opts
  ilMap <- liftIO $ newIORef S.empty
  return $ GlobalEnv opts ilMap

initCompileEnv :: GlobalEnv -> TopLevelModuleName  -> CompileEnv
initCompileEnv genv tlm = CompileEnv
  { globalEnv         = genv
  , currModule        = tlm
  , minRecordName     = Nothing
  , isNestedInType    = False
  , locals            = []
  , compilingLocal    = False
  , whereModules      = []
  , checkNames        = True
  , ctx               = initCtx
  }

initCompileState :: CompileState
initCompileState = CompileState

runC :: GlobalEnv -> TopLevelModuleName -> C a -> TCM (a, CompileOutput)
runC genv tlm c = evalRWST c (initCompileEnv genv tlm) initCompileState

moduleSetup :: GlobalEnv -> IsMain -> TopLevelModuleName -> Maybe FilePath -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup genv _ m mifile = do
  let opts = globalOptions genv
  -- we never compile primitive modules
  if any (`isPrefixOf` prettyShow m) primModules then pure $ Skip ()
  else do
    -- check whether the file needs to be recompiled
    uptodate <- case mifile of
      Nothing -> pure False
      Just ifile -> let ofile = moduleFileName opts m in
        liftIO =<< isNewerThan <$> ofile <*> pure ifile
    if uptodate then do
      reportSDoc "agda2hs.compile" 3 $ text "Module " <+> prettyTCM m <+> text " is already up-to-date"
      return $ Skip ()
    else do
      reportSDoc "agda2hs.compile" 3 $ text "Compiling module: " <+> prettyTCM m
      setScope . iInsideScope =<< curIF
      return $ Recompile m

compile
  :: GlobalEnv -> ModuleEnv -> IsMain -> Definition
  -> TCM (CompiledDef, CompileOutput)
compile genv tlm _ def =
  withCurrentModule (qnameModule qname)
    $ runC genv tlm 
    $ setCurrentRangeQ qname
    $ compileAndTag
  where
    opts = globalOptions genv
    qname = defName def

    tag []   = []
    tag code = [(nameBindingSite $ qnameName qname, code)]

    compileAndTag :: C CompiledDef
    compileAndTag = tag <$> do
      p <- processPragma qname

      reportSDoc "agda2hs.compile" 5  $ text "Compiling definition:" <+> prettyTCM qname
      reportSDoc "agda2hs.compile" 45 $ text "Pragma:" <+> text (show p)
      reportSDoc "agda2hs.compile" 65 $ text "Compiling definition:" <+> pretty (theDef def)


      case (p , theDef def) of
        (NoPragma            , _         ) -> return []
        -- (ExistingClassPragma , _         ) -> return []
        -- (UnboxPragma s       , Record{}  ) -> [] <$ checkUnboxPragma def
        -- (TransparentPragma   , Function{}) -> [] <$ checkTransparentPragma def
        -- (InlinePragma        , Function{}) -> [] <$ checkInlinePragma def
        -- (TuplePragma b       , Record{}  ) -> return []
        -- (CompileToPragma s   , Datatype{}) -> [] <$ checkCompileToDataPragma def s
        -- (CompileToPragma s   , Function{}) -> [] <$ checkCompileToFunctionPragma def s

        -- (ClassPragma ms      , Record{}  ) -> pure <$> compileRecord (ToClass ms) def
        -- (NewTypePragma ds    , Record{}  ) -> pure <$> compileRecord (ToRecord True ds) def
        -- (NewTypePragma ds    , Datatype{}) -> compileData True ds def
        -- (DefaultPragma ds    , Datatype{}) -> compileData False ds def
        -- (DerivePragma s      , _         ) | isInstance -> pure <$> compileInstance (ToDerivation s) def
        -- (DefaultPragma _     , Axiom{}   ) | isInstance -> pure <$> compileInstance (ToDerivation Nothing) def
        -- (DefaultPragma _     , _         ) | isInstance -> pure <$> compileInstance ToDefinition def
        -- (DefaultPragma _     , Axiom{}   ) -> compilePostulate def
        (DefaultPragma        , Function{}) -> compileFun True def
        (DefaultPragma        , Record{}  ) -> undefined -- pure <$> compileRecord (ToRecord False ds) def
        (DefaultPragma        , Datatype{}) -> undefined -- compileData False ds def

        _ -> agda2mbtErrorM $ text "Don't know how to compile" <+> prettyTCM (defName def)