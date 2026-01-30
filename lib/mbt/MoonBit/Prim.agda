{-# OPTIONS --no-auto-inline #-}

module MoonBit.Prim where 

open import Agda.Primitive          public renaming (Set to Type)
open import Agda.Builtin.Bool       public 
-- open import Agda.Builtin.Int        public 
open import Agda.Builtin.Nat        public -- Temporarily use agda Int for mbt Int 
open import Agda.Builtin.Char       public renaming (primCharToNat to c2n)
open import Agda.Builtin.Unit       public renaming (⊤ to Unit)
open import Agda.Builtin.Equality   public
open import Agda.Builtin.FromString public
open import Agda.Builtin.FromNat    public
open import Agda.Builtin.FromNeg    public
open import Agda.Builtin.String     public renaming (String to AgdaString)
open import Agda.Builtin.Word       public renaming (primWord64ToNat to w2n; primWord64FromNat to n2w)
open import Agda.Builtin.Strict     public
open import Agda.Builtin.List       public 

private variable
  @0 ℓ : Level
  a b c d e : Type
  f m s t : Type → Type

infix 10000 [_]2 [_]3 [_]4 [_]5 [_]6 [_]7 [_]8 [_]9 [_]10 [_]11 [_]12 [_]13 [_]14 [_]15 [_]16
-- [ A → B → C → D ]2 will compile to (A, B) -> (C) -> D
-- [ A → B → [ C → D → E ]2 ]2 will compile to (A, B) -> (C, D) -> E
[_]2 [_]3 [_]4 [_]5 [_]6 [_]7 [_]8 [_]9 [_]10 [_]11 [_]12 [_]13 [_]14 [_]15 [_]16 : Type ℓ → Type ℓ
[ x ]2  = x
[ x ]3  = x
[ x ]4  = x
[ x ]5  = x
[ x ]6  = x
[ x ]7  = x
[ x ]8  = x
[ x ]9  = x
[ x ]10 = x
[ x ]11 = x
[ x ]12 = x
[ x ]13 = x
[ x ]14 = x
[ x ]15 = x
[ x ]16 = x

_ : [ (Bool → Unit → Unit) ]2
_ = λ _ _ → tt

module Tuple where 

infix 3 _×_ _×_×_ _×_×_×_

infix -1 _,_ _,_,_ _,_,_,_

record _×_ (a b : Type) : Type where
  constructor _,_
  field
    fst : a
    snd : b
open _×_ public

record _×_×_ (a b c : Type) : Type where
  no-eta-equality; pattern
  constructor _,_,_
  field
    fst3 : a
    snd3 : b
    thd3 : c

record _×_×_×_ (a b c d : Type) : Type where
  no-eta-equality; pattern
  constructor _,_,_,_
  field
    fst4 : a
    snd4 : b
    thd4 : c
    fth4 : d