module Language.MoonBit.Pretty where 

import Language.MoonBit.Syntax
import Data.List (intercalate)

-- Pretty-printing for MoonBit syntax.

-- TODO : Make it better 
class Pretty t where 
  pretty :: t -> String 

sep :: String -> [String] -> String
sep = intercalate 

inParen :: String -> String -> String -> String
inParen l r s = l ++ s ++ r

instance Pretty Type where 
  pretty = \case 
    TyVar n -> n
    TyCon n [] -> n
    TyCon n ts -> n ++ inParen "[" "]" (sep ", " (map pretty ts))
    TyTup ts -> "(" ++ sep ", " (map pretty ts) ++ ")"
    TyArr args ret -> 
      inParen "(" ")" (sep ", " (map pretty args)) ++ " -> " ++ pretty ret
      