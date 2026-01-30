module Agda2Mbt.Pragma where 

import Data.List ( isPrefixOf )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import qualified Data.Text as Text

import Agda.Compiler.Backend
import Agda.Compiler.Common ( curIF )

import Agda.Syntax.Position
import Agda.TypeChecking.Pretty (text)

import Agda.Utils.FileName ( filePath )
import Agda.Utils.Maybe.Strict ( toLazy )

import Agda2Mbt.Compile.Types

import qualified Language.MoonBit as Mbt

pragmaName :: Text.Text
pragmaName = "AGDA2MBT"

data ParsedPragma
  = NoPragma
  | DefaultPragma
  deriving (Eq, Show)

processPragma :: QName -> C ParsedPragma
processPragma qn = liftTCM (getUniqueCompilerPragma pragmaName qn) >>= \case
  Nothing -> return NoPragma
  _ -> return DefaultPragma

