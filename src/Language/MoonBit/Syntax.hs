module Language.MoonBit.Syntax where

-- Abstract syntax for MoonBit.
-- Note that this is a simplified representation and may not cover all features of MoonBit.
-- Only the constructions necessary for extraction

-- TODO: Make it better.
type Name = String 
type TName = String 

type Tele = [(Name, Maybe Type)]  

data Context = Context 
  { ctxVars :: [Name]  
  , ctxTypes :: [TName]  
  } deriving (Show, Eq)

data Term =
    Var Name 
  | Lam Tele Term 
  | App Term [Type] [Term]
  | Match Term [Branch]
  | LocalFun [FunDecl] 
  | Segment String
  deriving (Show, Eq)

data Pattern = 
    PVar Name 
  | PCon Name [Pattern]
  deriving (Show, Eq)

data Branch = Branch 
  { bchLHS :: Pattern
  , bchGuard :: Maybe Term
  , bchRHS :: Term
  } deriving (Show, Eq)

data Type =
    TyVar TName 
  | TyCon TName [Type] 
  | TyTup [Type]
  | TyArr [Type] Type
  deriving (Show, Eq)

data Decl = DFun FunDecl | DEnum EnumDecl
  deriving (Show, Eq)

data FunDecl = FunDecl 
  { funName :: Name
  , funTArgs :: [TName]
  , funArgs :: Tele
  , funType :: Type
  , funBody :: Term
  } deriving (Show, Eq)

data EnumDecl = EnumDecl
  { enumName :: Name
  , enumTArgs :: [TName]
  , enumCons :: [Constr]
  } deriving (Show, Eq)

data Constr = Constr
  { constrName :: Name
  , constrArgs :: [Type]
  } deriving (Show, Eq)

-- Arity of (_, _) -> (_, _, _) -> (_) -> _ 
-- is [2, 3, 1]
type Arity = [Int]

-- TODO: Change type level arity to this 
-- type TyArity = Int