module Agda2Mbt.Main where

import Data.Maybe ( fromMaybe )
import qualified Data.Text as Text
import Data.Version ( showVersion )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import System.Environment ( getArgs )

import Agda.Main
import Agda.Compiler.Backend
import Agda.Utils.GetOpt

import Agda2Mbt.AgdaUtils
import Agda2Mbt.Compile.Types 
import Agda2Mbt.Compile


defaultOptions :: Options
defaultOptions = Options
  { optIsEnabled  = True
  , optOutDir     = Nothing
  , optConfigFile = Nothing
  }


backend :: Backend' Options GlobalEnv ModuleEnv ModuleRes (CompiledDef, CompileOutput)
backend = Backend'
  { backendName           = "agda2mbt"
  , backendVersion        = Nothing -- Just $ Text.pack $ showVersion version
  , options               = defaultOptions
  , commandLineFlags      =
      [ 
      --   Option ['d'] ["disable-backend"] (NoArg disableOpt)
      --     "Disable backend and fall back to vanilla Agda behaviour, \
      --     \without compilation (important for Emacs mode). \
      --     \Implied when run in interactive mode (with --interactive, --interaction or --interaction-json)."
      -- , Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
      --     "Write MoonBit code to DIR. (default: project root)"
      -- , Option [] ["config"] (ReqArg configOpt "FILE")
      --     "Provide additional configuration to agda2mbt with a YAML file."
      ]
  , isEnabled             = optIsEnabled
  , preCompile            = globalSetup
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = moduleSetup
  , postModule            = verifyAndWriteModule
  , compileDef            = compile
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  , backendInteractTop    = Nothing
  , backendInteractHole   = Nothing
  }
  where
    verifyAndWriteModule opts env isM m out = 
      undefined
      -- TODO
      -- verifyOutput opts env isM m out >> writeModule opts env isM m out
