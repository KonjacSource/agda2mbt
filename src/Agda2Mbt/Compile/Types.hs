{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Agda2Mbt.Compile.Types where

import Control.Applicative ( liftA2 )
import Control.Monad ( join )
import Control.Monad.Trans ( MonadTrans(lift) )
import Control.Monad.Trans.RWS.CPS (RWST(..), runRWST, rwsT )
import Control.Monad.Trans.Control (MonadTransControl(..))
import Control.Monad.State ( StateT(..) )
import Control.DeepSeq ( NFData(..) )

import Data.IORef
import Data.Maybe ( isJust )
import Data.Set ( Set )
import Data.Map ( Map )
import Data.String ( IsString(..) )

import Agda.Compiler.Backend
import Agda.Syntax.Position ( Range )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName )
import Agda.TypeChecking.Warnings ( MonadWarning )
import Agda.Utils.Null
import Agda.Utils.Impossible
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Abstract.Name ( nameConcrete )
import Agda.Syntax.Common hiding ( Ranged )
import Agda.TypeChecking.Monad.Base ( ContextEntry(..) )

import qualified Language.MoonBit as Mbt

data GlobalEnv = GlobalEnv
  { globalOptions :: Options
  , inlineSymbols :: IORef (Set QName)
  }

type NameOrT = Either Mbt.Name Mbt.TName

type QNameIx = String
type NameIx  = String

qnameIx :: QName -> QNameIx
qnameIx = prettyShow

nameIx :: Name -> NameIx 
nameIx = prettyShow . nameConcrete 

data Ctx = Ctx 
  { qnameArity :: Map QNameIx Mbt.Arity -- indexed by prettyShow of QName
  , nameArity  :: Map NameIx Mbt.Arity -- indexed by nameIx of Name
  , qnameMap   :: Map QNameIx NameOrT -- indexed by prettyShow of QName
  , nameMap    :: Map NameIx NameOrT -- indexed by nameIx of Name
  , varScope   :: Set Mbt.Name 
  -- ^ variables of localNames
  , tyVarScope :: Set Mbt.TName
  -- ^ type variables in scope}
  }

type ModuleEnv   = TopLevelModuleName
type ModuleRes   = ()
type CompiledDef = [Ranged [Mbt.Decl]]
type Ranged a    = (Range, a)

data CompiledDom
  = DomType Mbt.Type
    -- ^ To a MoonBit type
  -- | DomConstraint (Hs.Asst ())
  -- | DomForall (Maybe (Hs.TyVarBind ()))
  | DomDropped
    -- ^ To nothing (e.g. erased proofs)


data Options = Options
  { optIsEnabled  :: Bool
  , optOutDir     :: Maybe FilePath
  , optConfigFile :: Maybe FilePath
  }

-- Required by Agda-2.6.2, but we don't really care.
instance NFData Options where
  rnf _ = ()

-- | Names of local declarations (in @where@ blocks).
type LocalDecls = [QName]

data CompileEnv = CompileEnv
  { globalEnv :: GlobalEnv
  -- ^ global environment
  , currModule :: TopLevelModuleName
  -- ^ the current module we are compiling
  , minRecordName :: Maybe ModuleName
  -- ^ keeps track of the current minimal record we are compiling
  , isNestedInType :: Bool
  -- ^ if we're inside an argument of a type and need an explicit forall
  --   Useless for MoonBit, but kept for possible future use.
  , locals :: LocalDecls
  -- ^ keeps track of the current clause's where declarations
  , compilingLocal :: Bool
  -- ^ whether we are currently compiling a where clause or pattern-matching lambda
  , whereModules :: [ModuleName]
  -- ^ the where-blocks currently in scope. Hack until Agda adds where-prominence
  , checkNames :: Bool
  -- ^ whether we should check validity of compiled names
  , ctx :: Ctx
  }

-- | Output produced during the compilation of a module
data CompileOutput = CompileOutput
  { -- Nothing yet
  }

instance Semigroup CompileOutput where
  _ <> _ = CompileOutput

instance Monoid CompileOutput where
  mempty = CompileOutput

-- | State used while compiling a single module.
data CompileState = CompileState
  { -- Nothing yet
  }

-- NOTE: C should really be a newtype for abstraction's sake
--       but none of the Agda instances are defined for RWST(.CPS)

-- | Compilation monad.
type C = RWST CompileEnv CompileOutput CompileState TCM

-- some missing instances from the Agda side
instance HasFresh i => MonadFresh i C
instance MonadTCState C
instance Monoid w => MonadTransControl (RWST r w s) where
  type StT (RWST r w s) a = (a, s, w)
  liftWith f = rwsT $ \r s -> fmap (, s, mempty) (f $ \t -> runRWST t r s)
  restoreT mSt = rwsT $ \_ _ -> mSt
instance MonadTCEnv C
instance HasOptions C
instance ReadTCState C
instance MonadTCM C
instance MonadTrace C where
  traceClosureCall c f = rwsT $ \ r s -> traceClosureCall c $ runRWST f r s
instance MonadInteractionPoints C where
instance MonadDebug C where
instance HasConstInfo C where
instance MonadReduce C where
instance MonadAddContext C where
instance HasBuiltins C where
instance MonadBlock C where
  catchPatternErr h m = rwsT $ \ r s ->
    let run x = runRWST x r s in catchPatternErr (run . h) (run m)
instance MonadStConcreteNames C where
  runStConcreteNames m = rwsT $ \r s -> runStConcreteNames $ StateT $ \ns -> do
    ((x, ns'), s', w) <- runRWST (runStateT m ns) r s
    pure ((x, s', w), ns')
instance MonadWarning C where
instance IsString a => IsString (C a) where fromString = pure . fromString
instance PureTCM C where
instance Null a => Null (C a) where
  empty = lift empty
  null = __IMPOSSIBLE__
instance Semigroup a => Semigroup (C a) where (<>) = liftA2 (<>)

