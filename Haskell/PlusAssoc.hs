{-# LANGUAGE TypeOperators, TypeFamilies, GADTs #-}
module PlusAssoc where
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

    -- Helpers

    -- | For any n, n = n.
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