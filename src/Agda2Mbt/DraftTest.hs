module Agda2Mbt.DraftTest where

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
import Agda2Mbt.Compile

import Language.MoonBit as Mbt

import Data.Foldable
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Except
import Control.Monad 

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Primitive
import Agda.Syntax.Abstract as A
import Agda.Syntax.Concrete as C
import Agda.Syntax.Internal as I
import Agda.Syntax.Common
import Agda.Syntax.Position
import Agda.TheTypeChecker
import Agda.Interaction.Imports
import Agda.Interaction.Options ( defaultOptions )
import Agda.Utils.FileName
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Errors
import Agda.TypeChecking.Pretty  ( prettyTCM, (<+>), text )
import Agda.Syntax.Translation.InternalToAbstract (reify)  
import Agda.Syntax.Translation.AbstractToConcrete (abstractToConcrete_)  
import Agda.Interaction.BasicOps
import Agda.Syntax.Scope.Monad (resolveName)  
import Agda.TypeChecking.Datatypes
import Agda.Syntax.Scope.Base
import Agda.Utils.Impossible
import Agda.Utils.List1
import Agda.Syntax.Common.Pretty ( render )
import Agda.Version  
import Agda.TypeChecking.Rules.Term
import Agda.Interaction.Base

import Agda.TypeChecking.MetaVars  
import Agda.TypeChecking.Monad.MetaVars  
import Agda.Syntax.Info  
import Agda.TypeChecking.Monad.Signature (HasConstInfo)  
import Agda.TypeChecking.Monad.Options  
import Agda.Interaction.Options (defaultOptions)
import Agda.Syntax.Translation.ConcreteToAbstract (concreteToAbstract_)  
import Agda.TypeChecking.Monad.MetaVars 
import Agda.Interaction.EmacsTop
import Agda.Interaction.Output
import Agda.Syntax.Common.Pretty

printVersion :: IO ()  
printVersion = putStrLn $ "You are using Agda " ++ version

createInteractionPoint :: Range -> I.Type -> TCM (InteractionId, MetaId, I.Term)  
createInteractionPoint range typ = do  
  -- Register the interaction point  
  ii <- registerInteractionPoint True range Nothing  
  -- Create a meta variable for it  
  (mid, term) <- newQuestionMark ii CmpLeq typ  
  return (ii, mid, term)


checkTest :: TCM Interface
checkTest = do 
  setCommandLineOptions defaultOptions
  let fp = "DraftTest/Test.agda"
  afp <- liftIO $ absolute fp
  src <- parseSource =<< srcFromPath afp
  -- typeCheckMain :: Mode -> Source -> TCM CheckResult
  -- Mode = TypeCheck | ScopeCheck 
  CheckResult { crInterface = i , crWarnings = warnings } <- 
    typeCheckMain TypeCheck src `catchError ` \e -> do 
      e <- prettyTCM e 
      liftIO $ putStrLn $ render e 
      error "!"
  forM_ warnings $ \ w -> do 
    w <- prettyTCM w
    liftIO $ putStrLn $ render w
  pure i

global :: TCM GlobalEnv
global = globalSetup $ Options True Nothing Nothing

inter :: IO () 
inter = runTCMTop' $ do 
  i <- checkTest
  withScope_ (iInsideScope i) $ do
    str <- liftIO $ do 
      putStr "Agda type: " 
      getLine 
    typc <- parseExpr noRange str 
    typ <- concreteToAbstract_ typc
    typ' <- isType_ typ
    typ'_doc <- prettyTCM typ' 
    liftIO $ putStrLn $ "Checked type: " ++ render typ'_doc
    Just tlm <- currentTopLevelModule 
    global <- global
    (a, _) <- runC global tlm $ do 
      compileType 1 (unEl typ')
    liftIO $ putStrLn $ Mbt.pretty a



-- examplMeta :: TCM () 
-- examplMeta = do 
--   i <- checkTest
--   withScope_ (iInsideScope i) $ do 
--     ii <- registerInteractionPoint True noRange Nothing 
--     typc <- parseExpr noRange "Bool"
--     typ <- concreteToAbstract_ typc
--     typ' <- isType_ typ
--     (mid, term) <- newQuestionMark ii CmpLeq typ'
--     mdoc <- prettyTCM mid 
--     tdoc <- prettyTCM typ'
--     liftIO $ putStrLn $ "Created meta variable with id: ?" ++ render mdoc ++ "  : " ++ render tdoc
--     liftIO $ putStrLn "All metas: "
--     openMetas <- getOpenMetas  
--     forM_ openMetas $ \m -> do  
--       mdoc <- prettyTCM m  
--       liftIO $ putStrLn $ "\t" ++ render mdoc
--     im <- getInteractionIdsAndMetas
--     liftIO $ putStrLn $ "{____________________"
--     forM_ im $ \(i, m) -> do 
--       mdoc <- prettyTCM m 
--       idoc <- prettyTCM i 
--       liftIO $ putStrLn $ "| " ++ render mdoc ++ " <-> " ++ render idoc 
--     true <- parseExprIn ii noRange "true"
--     giveExpr WithoutForce (Just ii) mid true
--     removeInteractionPoint ii
--     liftIO $ putStrLn "After giving true to the meta, open metas: "
--     openMetas2 <- getOpenMetas
--     forM_ openMetas2 $ \m -> do  
--       mdoc <- prettyTCM m  
--       liftIO $ putStrLn $ "\t" ++ render mdoc
--     im <- getInteractionIdsAndMetas
--     liftIO $ putStrLn $ "____________________}"
--     forM_ im $ \(i, m) -> do 
--       mdoc <- prettyTCM m 
--       idoc <- prettyTCM i 
--       liftIO $ putStrLn $ "| " ++ render mdoc ++ " <-> " ++ render idoc
--       ip <- lookupInteractionPoint i  
--       liftIO $ putStrLn $ show (ipSolved ip) 
    

-- inter :: IO () 
-- inter = runTCMTop' $ do 
--   i <- checkTest
--   withScope_ (iInsideScope i) $ do
--     str <- liftIO $ do 
--       putStr "give me a type plz: " 
--       getLine 
--     typc <- parseExpr noRange str 
--     typ <- concreteToAbstract_ typc
--     typ' <- isType_ typ 
--     ii <- registerInteractionPoint True noRange Nothing 
--     (mid, term) <- newQuestionMark ii CmpLeq typ'
--     term_doc <- prettyTCM term
--     liftIO $ putStrLn $ "Created meta: ?" ++ render term_doc
--     openMetas <- getInteractionIdsAndMetas
--     eachMeta openMetas
--     liftIO $ putStrLn "All metas solved!"
--     term_doc <- prettyTCM term
--     liftIO $ putStrLn $ "Solved: " ++ render term_doc
--   where 
--     eachMeta [] = pure ()
--     eachMeta openMetas = do
--       forM_ openMetas $ \(i, m) -> do 
--         m_doc <- prettyTCM m
--         i_doc <- prettyTCM i
--         liftIO $ putStrLn $ "| Meta: " ++ render m_doc ++ " with interaction id: " ++ render i_doc
--       forM_ openMetas $ \(i, m) -> do
--         ip <- lookupInteractionPoint i  
--         mv <- lookupLocalMeta m 
--         enterClosure mv $ \_ -> do 
--           rces <- contextOfMeta i AsIs
--           m_doc <- prettyTCM m
--           ctx_doc <- prettyResponseContext i False rces
--           liftIO $ do 
--             putStrLn $ "Solving meta: " ++ render m_doc
--             putStrLn $ "In context: " 
--             putStrLn $ render ctx_doc
--             putStrLn $ "Goal: "
--           t1 <- metaType m
--           t2 <- getMetaType m 
--           t3 <- getMetaTypeInContext m 
--           t4 <- typeOfMeta AsIs i 
--           t1_doc <- prettyTCM t1
--           t2_doc <- prettyTCM t2
--           t3_doc <- prettyTCM t3
--           -- let t4_doc = pretty t4
--           liftIO $ do 
--             -- putStrLn $ "\tmetaType: " ++ render t1_doc
--             -- putStrLn $ "\tgetMetaType: " ++ render t2_doc
--             putStrLn $ "\tgetMetaTypeInContext: " ++ render t3_doc
--             -- putStrLn $ "\ttypeOfMeta: " ++ render t4_doc
--             pure ()
--           str <- liftIO $ do 
--             putStr "give me a term plz: "
--             getLine
--           termc <- parseExprIn i noRange str
--           giveExpr WithoutForce (Just i) m termc
--           removeInteractionPoint i
--           liftIO $ putStrLn "----------------------------------"
--       openMetas' <- getInteractionIdsAndMetas
--       eachMeta openMetas'

