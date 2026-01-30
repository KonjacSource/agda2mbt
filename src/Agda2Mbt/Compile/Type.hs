module Agda2Mbt.Compile.Type where

import Control.Arrow ( (>>>) )
import Control.Monad ( forM, when, unless )
import Control.Monad.Trans ( lift )
import Control.Monad.Reader ( asks )
import Data.List
import Data.Maybe ( mapMaybe, isJust )
import qualified Data.Set as Set ( singleton )
import Text.Read (readMaybe)

import Agda.Compiler.Backend hiding ( Args )

import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.Syntax.Common.Pretty ( prettyShow, render )

import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Reduce ( reduce, unfoldDefinitionStep, instantiate )
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Telescope

import Agda.Utils.Impossible ( __IMPOSSIBLE__ )
import Agda.Utils.List ( downFrom )
import Agda.Utils.Maybe ( ifJustM, fromMaybe )
import Agda.Utils.Monad ( ifM, whenM, unlessM, and2M, or2M )
import Agda.Utils.Size ( Sized(size) )
import Agda.Utils.Functor ( ($>) )

import Agda2Mbt.Compile.Name
import Agda2Mbt.Compile.Types
import Agda2Mbt.Compile.Utils
import Agda2Mbt.Compile.Var
import Agda2Mbt.AgdaUtils

import qualified Language.MoonBit as Mbt
import Debug.Trace

isSpecialType :: QName -> Maybe (Elims -> C Mbt.Type)
isSpecialType qn = case prettyShow qn of
  "MoonBit.Prim.Tuple._×_"    -> Just tupleType
  "MoonBit.Prim.Tuple._×_×_"  -> Just tupleType
  str | Just rest <- stripPrefix "MoonBit.Prim.[_]" str
      , Just n <- readMaybe rest -> Just $ arityFun qn n
  -- "MoonBit.Prim.Sigma.Σ"     -> Just tupleType
  -- "MoonBit.Prim.Erase.Erase" -> Just unitType
  _                           -> Nothing

arityFun :: QName -> Int -> Elims -> C Mbt.Type
arityFun qn n es = do
  let Just as = allApplyElims es
  case as of
    [_, t] -> compileType n (unArg t)
    as -> agda2mbtError =<< text "Bad arity function:" <+> prettyTCM (Def qn es)


-- | Compile all the elims into a n-uple.
tupleType :: Elims -> C Mbt.Type
tupleType es = do
  let Just as = allApplyElims es
  ts <- mapM (compileType 0 . unArg) as
  return $ Mbt.TyTup ts

compileType :: Int -> Term -> C Mbt.Type
compileType arity t = do

  reportSDoc "agda2hs.compile.type" 12 $ text "Compiling type" <+> prettyTCM t
  reportSDoc "agda2hs.compile.type" 22 $ text "Compiling type" <+> pretty t

  toInline <- getInlineSymbols
  t <- locallyReduceDefs (OnlyReduceDefs toInline) $ reduce t

  whenM (isErasedBaseType t) fail

  case t of
    Pi a b -> do
      reportSDoc "agda2hs.compile.type" 13 $ text "Compiling pi type (" <+> prettyTCM (absName b)
        <+> text ":" <+> prettyTCM a <+> text ") -> " <+> underAbstraction a b prettyTCM
      let compileB arity' = underAbstraction a b (compileType arity' . unEl)
      compileDomType (absName b) a >>= \case
        -- Though I write hsX, You know I mean mbtX
        DomType mbtA
          | arity == 0 -> __IMPOSSIBLE__
          | arity == 1 -> do
              mbtB <- compileB 1
              return $ Mbt.TyArr [mbtA] mbtB
          | otherwise  -> do
              mbtB <- compileB (arity - 1)
              case mbtB of
                Mbt.TyArr mbtBP mbtB' -> 
                  return $ Mbt.TyArr (mbtA : mbtBP) mbtB'
                _ -> agda2mbtErrorM $ text "Arity mismatch when compiling type:" <+> prettyTCM t
        DomDropped -> compileB arity

    Def f es -> maybeUnfoldCopy f es (compileType arity) $ \f es -> do
      def <- getConstInfo f
      if | not (usableModality def) ->
            agda2mbtErrorM $
                  text "Cannot use erased definition" <+> prettyTCM f
              <+> text "in Haskell type"
         | Just semantics <- isSpecialType f -> setCurrentRange f $ semantics es
         | Just args <- allApplyElims es -> do
              (f, arity) <- compileQNameTy f
              vs <- compileTypeArgs (defType def) args
              return $ Mbt.TyCon  f vs
         | otherwise -> fail

    Var x es | Just args <- allApplyElims es -> do
      CtxVar _ ti <- lookupBV x
      unless (usableModality ti) $ agda2mbtErrorM $
            text "Cannot use erased variable" <+> prettyTCM (var x)
        <+> text "in MoonBit type"
      vs <- compileTypeArgs (unDom ti) args
      
      (x, arity)  <- compileDBTyVar x

      unless (null arity && null vs) $ agda2mbtErrorM $
        text "No HKTs in MoonBit: " <> text (prettyShow x)

      return $ Mbt.TyVar x

    Lam argInfo restAbs -> do
      body <- underAbstraction_ restAbs (compileType arity) 

      -- TODO: we should also drop lambdas that can be erased based on their type
      -- (e.g. argument is of type Level/Size or in a Prop) but currently we do
      -- not have access to the type of the lambda here.
      if hasQuantity0 argInfo then return body
         -- Rewrite `\x -> (a -> x)` to `(->) a`
        --  | Hs.TyFun _ a (Hs.TyVar _ y) <- body
        --  , y == x0 -> return $ Hs.TyApp () (Hs.TyCon () $ Hs.Special () $ Hs.FunCon ()) a
         -- Rewrite `\x -> f x` to `f`
        --  | Hs.TyApp _ f (Hs.TyVar _ y) <- body
        --  , y == x0 -> return f
        else agda2mbtErrorM $ text "Not supported: type-level lambda" <+> prettyTCM t

    _ -> fail
  where fail = agda2mbtErrorM $ text "Bad MoonBit type:" <?> prettyTCM t

compileTypeArgs :: Type -> Args -> C [Mbt.Type]
compileTypeArgs ty [] = pure []
compileTypeArgs ty args = undefined

data DomOutput =
    -- DOInstance 
    DODropped
  | DOType
  | DOTerm

compileDom :: Dom Type -> C DomOutput
compileDom a = do
  isErasable <- pure (not $ usableModality a) `or2M` canErase (unDom a)
  isType <- endsInSort (unDom a)
  return $ if
    | isErasable        -> DODropped
    | isType            -> DOType
    | otherwise         -> DOTerm

-- | Compile a function type domain.
-- A domain can either be:
--
-- - dropped if the argument is erased.
-- - added as a class constraint.
-- - added as a type parameter
-- - kept as a regular explicit argument.
compileDomType :: ArgName -> Dom Type -> C CompiledDom
compileDomType x a =
  compileDom a >>= \case
    DODropped  -> pure DomDropped
    DOType     -> do
      -- We compile (non-erased) type parameters to an explicit forall if they
      -- come from a module parameter or if we are in a nested position inside the type.
      agda2mbtErrorM $ text "MoonBit does not support RankNTypes (N > 1)" 
    DOTerm     -> fmap DomType . withNestedType . compileType 1 . unEl $ unDom a
