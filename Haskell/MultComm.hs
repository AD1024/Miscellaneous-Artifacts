{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}
module MultComm where

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)


-- This will be helpful
plus' :: Equal n m -> Natural a -> Equal (n :+: a) (m :+: a)
plus' = undefined

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS e) = EqlS (symmetric e)

plusRefl :: Natural a -> Natural b -> Equal (a :+: b) (a :+: b)
plusRefl NumZ b = reflexive b
plusRefl (NumS a) b = EqlS (plusRefl a b)

-- This is the proof that the kata requires.
-- | a + (b + c) = (a + b) + c
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ b c = plusRefl b c
plusAssoc (NumS a) b c = EqlS (plusAssoc a b c)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
    -- Transitivity
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ              = EqlZ
transitive (EqlS eql) (EqlS eql') = EqlS (transitive eql eql')

-- n + 0 = n
plusZ :: Natural a -> Natural Z -> Equal (a :+: Z) (a)
plusZ NumZ NumZ     = EqlZ
plusZ (NumS a) NumZ = EqlS (plusZ a NumZ)

-- n + S m = S (n + m)
plusS :: Natural a -> Natural b -> Equal (a :+: (S b)) (S (a :+: b))
plusS NumZ NumZ     = EqlS EqlZ
plusS NumZ (NumS b) = EqlS (plusS NumZ b)
plusS (NumS a) NumZ = EqlS (plusS a NumZ)
plusS (NumS a) b    = EqlS (plusS a b)

-- a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ     = EqlZ
plusCommutes NumZ (NumS b) = EqlS (symmetric $ plusZ b NumZ)
plusCommutes (NumS a) NumZ = EqlS (plusZ a NumZ)
plusCommutes (NumS a) b    = transitive 
                            (symmetric ( plusS a b ))
                            (symmetric 
                                (transitive 
                                (plusS b a)
                                (symmetric 
                                    (plusCommutes a (NumS b)))))

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = EqlZ
zeroComm (NumS x) = zeroComm x

plusEq' :: Natural a -> Natural b -> Natural c -> Equal b c -> Equal (c :+: a) (b :+: a)
plusEq' NumZ b c eq = symmetric $ transitive (transitive (plusZ b NumZ) eq) (symmetric $ plusZ c NumZ)
plusEq' (NumS a) b c eq =             (plusS c a) 
                        `transitive` (EqlS (plusEq' a b c eq)) 
                        `transitive` (symmetric $ plusS b a) 

natPlus :: Natural a -> Natural b -> Natural (a :+: b)
natPlus NumZ b = b
natPlus (NumS a) b = NumS (natPlus a b)

natMult :: Natural a -> Natural b -> Natural (a :*: b)
natMult NumZ _ = NumZ
natMult (NumS a) b = b `natPlus` (natMult a b)

plusEq :: Natural a -> Equal b c -> Equal (a :+: b) (a :+: c)
plusEq NumZ eq = eq
plusEq (NumS a) eq = EqlS (plusEq a eq)

eqPlus :: Natural a -> Equal b c -> Equal (b :+: a) (c :+: a)
eqPlus a EqlZ = reflexive a
eqPlus a (EqlS eq) = EqlS (eqPlus a eq)

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ b = transitive EqlZ (zeroComm b)
timesComm (NumS a) NumZ = transitive (symmetric $ zeroComm a) EqlZ
timesComm sa@(NumS a) sb@(NumS b) = EqlS(
  (plusEq b (timesComm a sb)) `transitive`
  (plusEq b (plusEq a (timesComm b a))) `transitive`
  (plusAssoc b a (a `natMult` b)) `transitive`
  (eqPlus (a `natMult` b) (plusCommutes b a)) `transitive`
  (symmetric $ plusAssoc a b (a `natMult` b)) `transitive`
  plusEq a (timesComm sa b))
