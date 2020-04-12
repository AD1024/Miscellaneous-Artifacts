module UTLC where

import Prelude hiding (pred, or, and)

-- from :: Int -> D
-- from x = N $ from' x
--     where
--         from' x = if x <= 0 then O else S $ from' (x - 1)

-- c :: Int -> D
-- c = fromPeano . from

-- p :: D -> Int
-- p = showPeano . toPeano

data Nat = O | S Nat

-- Uncomment the following line to add your defintion for D
data D = N Nat | Lam (D -> D) | Pi D D

-- app takes an untyped function and an untyped term and calculate
-- the resulting terms after application
app :: D -> D -> D
app f x = case f of
            N _ -> error "can't apply this term"
            Pi _ _ -> error "can't apply this term"
            Lam f' -> f' x

-- lam takes a function in higher-order abstract syntax (HOAS) and returns
-- an untyped terms representing the function
lam :: (D -> D) -> D
lam f = Lam f

-- Now let's define church's number
zero :: D
zero = lam $ \_ -> lam $ \z -> z

suc :: D
suc = lam $ \n -> lam $ \s -> lam $ \z -> s `app` (n `app` s `app` z)

-- Some other operations we can define
one :: D
one = app suc zero

plus :: D
plus = lam $ \m -> lam $ \n -> lam $ \s -> lam $ \z -> m `app` s `app` (n `app` s `app` z)

zz :: D
zz = Pi zero zero

ss :: D
ss = lam $ \p -> case p of
                    Pi _ y -> Pi y (plus `app` one `app` y)
                    _       -> undefined

pred :: D
pred = lam $ \n -> case n `app` ss `app` zz of
                    Pi x _ -> x
                    _      -> undefined

mult :: D
mult = lam $ \m -> lam $ \n -> m `app` (plus `app` n) `app` zero

-- We can even write out the defintion of y-combinator!
ycomb :: D
ycomb = lam $ \f -> app (lam $ \x -> f `app` (lam $ \y -> (x `app` x `app` y))) 
                        (lam $ \x -> f `app` (lam $ \y -> (x `app` x `app` y)))

-- And true and false
true :: D
true = lam $ \t -> lam $ \_ -> t

false :: D
false = lam $ \_ -> lam $ \f -> f

-- What about if-then-else?
ite :: D
ite = lam $ \cond -> 
            lam $ \lb -> 
            lam $ \rb -> cond `app` lb `app` rb

-- with ite, we can define our lovely
-- logical and/or operators
and :: D
and = lam $ \a -> lam $ \b ->
    ite `app` a `app` b `app` false

or :: D
or = lam $ \a -> lam $ \b ->
    ite `app` a `app` true `app` b

-- Here is the predicate to test if n is zero
iszero :: D
iszero = lam $ \n -> n `app` (lam $ \_ -> false) `app` true

-- This converts the Church encoding to the Peano encoding of natural numbers
toPeano :: D -> D
toPeano (N _) = error "not a Church number!"
toPeano n = n `app` (lam suc_nat) `app` (N O)
    where
        suc_nat (N n) = N (S n)
        suc_nat _ = error "stuck!"
          
-- This converts the Peano encoding to the Church encoding of natural numbers
fromPeano :: D -> D
fromPeano (N O) = zero
fromPeano (N (S n)) = app suc (fromPeano (N n))
fromPeano _ = error "not a Peano number!"

showPeano :: D -> Int
showPeano = toInt
    where
        toInt :: D -> Int
        toInt (N O) = 0
        toInt (N (S x)) = 1 + (toInt (N x))
        toInt _ = undefined