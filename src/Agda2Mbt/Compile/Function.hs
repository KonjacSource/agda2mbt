module Agda2Mbt.Compile.Function where

import Control.Arrow ( (***) )
import Control.Monad ( (>=>), filterM, forM_ )
import Control.Monad.Reader ( asks, local )

import Data.Generics
import Data.List
import Data.Maybe ( fromMaybe, isJust )
import qualified Data.Text as Text

import Agda.Compiler.Backend hiding (Args)
import Agda.Compiler.Common

import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Pattern ( patternToTerm )
import Agda.Syntax.Literal
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Scope.Monad ( isDatatypeModule )

import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Telescope ( telView, mustBePi, piApplyM )
import Agda.TypeChecking.Sort ( ifIsSort )
import Agda.TypeChecking.Datatypes ( getConType, isDataOrRecord )
import Agda.TypeChecking.Records ( getDefType )
import Agda.TypeChecking.Reduce ( reduce )

import Agda.Utils.Functor ( (<&>), dget)
import Agda.Utils.Impossible ( __IMPOSSIBLE__ )
import Agda.Utils.Lens ((^.))
import Agda.Utils.List
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Size ( Sized(size) )

import Agda2Mbt.Compile.Types
import Agda2Mbt.Compile.Utils
import Agda2Mbt.Compile.Name
import Agda2Mbt.Compile.Var 
import Agda2Mbt.Compile.Type 
import Agda2Mbt.Compile.Term

import Language.MoonBit as Mbt

compileFun 
  :: Bool -- ^ Allow polymorphic?
  -> Definition -> C [Mbt.Decl]
compileFun isPoly def@Defn{..} = do 
  reportSDoc "agda2mbt.compile" 6 $ "Compiling function: " <+> prettyTCM defName
  
  undefined