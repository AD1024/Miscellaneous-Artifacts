{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (to, from)
                        where
                            to f = \b b' -> ab (f (ba b) (ba b'))
                            from f = \a a' -> ba (f (ab a) (ab a'))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
    zero = O
    successor = S
    nat base step k = 
        case k of
            O -> base
            S k' -> step k'
    iter base ind k =
        case k of
            O -> base
            S k' -> ind $ iter base ind k'
    plus x y = case x of
                O -> y
                S x' -> S $ plus x' y
    minus x y = case x of
                O -> O
                o@(S x') -> case y of
                                O -> o
                                S y' -> minus x' y'
    mult x y = case x of
                O -> O
                S x' -> plus y (mult x' y)
    
    pow x y = case y of
                O -> S O
                S y' -> mult x (pow x y')


-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero = []
  successor n = () : n
  nat base step n = 
      case n of
        [] -> base
        _ : xs -> step xs
  iter base ind n = 
      case n of
        [] -> base
        (_ : xs) -> ind $ iter base ind xs
  
  plus = (++)
  minus x y 
    | length x > length y = take (length x - length y) x
    | otherwise           = []
  
  mult x y =
      case x of
        [] -> []
        (_ : xs) -> (mult xs y) ++ y
  
  pow x y =
      case y of
        [] -> [()]
        (_ : ys) -> mult x (pow x ys)

-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

  zero = Scott const
  successor n = Scott $ \_ f -> f n
  nat base step n = (runScott n) base step
  iter base step n = (runScott n) base $ iter (step base) step

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }

iszero (Church n) = n (const False) True

zz :: (Church, Church)
zz = (zero, zero)

ss :: (Church, Church) -> (Church, Church)
ss (_, now) = (now, successor now)

churchPred :: Church -> Church
churchPred (Church n)= fst $ n ss zz

instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero = Church (\s z -> z)
  successor = \(Church n) -> Church (\s z -> s $ n s z)
  nat base step n = if iszero n then base else step (churchPred n)

  iter base step (Church n) = n step base

  plus (Church a) (Church b) = Church $ \s z -> a s (b s z)

  mult (Church a) (Church b) = Church $ a . b
  pow (Church a) (Church b)  = Church (b a)
  minus a (Church b) = b churchPred a