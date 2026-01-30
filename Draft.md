# Compile Agda to MoonBit

Basically, as same as Agda2hs, but MoonBit do not use curried form of functions, so we need to store the arity of functions.

```agda

data List (A : Type) : Type where 
  [] : List A
  _::_ : A → List A → List A 

{-# COMPILE AGDA2MBT List #-}
-- All constructors have full arity

map : {@0 A B : Type} → (A → B) → List A → List B
map f []       = []
map f (x :: xs) = f x :: map f xs

-- For toplevel functions, the arity is simply the number of arguments on lhs.

zipMap : {@0 A B C : Type} → (A → B → C) → List A → List B → List C
zipMap f []       ys        = []    
zipMap f (x :: xs) []       = []    
zipMap f (x :: xs) (y :: ys) = f x y :: zipMap f xs ys

-- How do we know the arity of `f` in the above example?

-- Sol1. Specify the arity manually.

zipMap : (f : A → B → C) → List A → List B → List C
zipMap f []       ys        = []    
zipMap f (x :: xs) []       = []    
zipMap f (x :: xs) (y :: ys) = f x y :: zipMap f xs ys

{-# COMPILE AGDA2MBT zipMap.f 2 #-}

-- Sol2. Use a axuiliary type to store the arity information.
zipMap : [A → B → C]2 → List A → List B → List C
zipMap f []       ys        = []
zipMap f (x :: xs) []       = []
zipMap f (x :: xs) (y :: ys) = f x y :: zipMap f xs ys

-- Sol2 is better , since we may want the following translation.

f : [A → B → C]2 → A → B → C
f x = x

-- trans to 

-- mbt
fn f(x : (A, B) -> C) -> (A) -> (B) -> C {
  a => b => x (a, b)
}

-- Well 
f : [A → B → C]2 → [A → B → C]2
f x = x

-- trans to 

-- mbt
fn f(x : (A, B) -> C) -> (A, B) -> C {
  x
}
```

This requires us to generate (readable) local names for lambda variables(track the scope for mbt vars).
