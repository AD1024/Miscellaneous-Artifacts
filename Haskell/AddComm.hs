{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module AddComm where 
    data Z
    data S n
    
    data Natural :: * -> * where
        NumZ :: Natural Z
        NumS :: Natural n -> Natural (S n)
    
    data Equal :: * -> * -> * where
        EqlZ :: Equal Z Z
        EqlS :: Equal n m -> Equal (S n) (S m)
    
    type family (:+:) (n :: *) (m :: *) :: *
    type instance Z :+: m   = m
    type instance S n :+: m = S (n :+: m)
    
    -- Reflexivity
    reflexive :: Natural n -> Equal n n
    reflexive NumZ     = EqlZ
    reflexive (NumS n) = EqlS $ reflexive n
    
    -- Symmetry
    symmetric :: Equal a b -> Equal b a
    symmetric EqlZ       = EqlZ
    symmetric (EqlS eql) = EqlS $ symmetric eql
    
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
    -- plusCommutes NumZ (NumS b) = EqlS (symmetric $ plusZ b NumZ)
    -- plusCommutes (NumS a) NumZ = EqlS (plusZ a NumZ)
    plusCommutes (NumS a) b    = transitive 
                                (symmetric ( plusS a b ))
                                (symmetric 
                                    (transitive 
                                    (plusS b a)
                                    (symmetric 
                                        (plusCommutes a (NumS b)))))
