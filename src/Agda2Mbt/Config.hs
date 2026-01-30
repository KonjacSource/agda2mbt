module Agda2Mbt.Config where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import GHC.Generics

import Data.Functor ( (<&>) )
import Data.Foldable ( fold )
import Data.Maybe ( fromMaybe )
import Data.Aeson ( FromJSON(parseJSON), withObject, (.:), (.:?), (.!=) )
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml

import Agda.TypeChecking.Monad.Base ( TCM )
import Agda.Syntax.Common.Pretty

import Agda2Mbt.Compile.Types

-- TODO: Define the config structure for Agda2Mbt
checkConfig :: Options -> TCM Options
checkConfig opts = return opts

