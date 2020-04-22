module Imperative where

    import Control.Monad (ap)
    import Control.Applicative (liftA)

    data ProgState a = State ((Int -> Integer, Int) -> (a, Int -> Integer, Int))

    data Variable = Var Int | Const Integer

    instance Monad ProgState where
        return = pure
        (State sta) >>= f = State $ \st ->
                                        let
                                            (x, st', n') = sta st in
                                        let State stb = f x in
                                            stb (st', n')

    instance Applicative ProgState where
        pure x = State $ \(st, n) -> (x, st, n)
        (<*>) = ap

    instance Functor ProgState where
        fmap = liftA

    def :: ProgState Variable -> Integer
    def (State st) = case st (\_ -> 0, 0) of
                        (Var x, st', _) -> st' x
                        (Const n, _, _) -> n

    while :: Variable -> (Integer -> Bool) -> ProgState () -> ProgState ()
    while v f (State st) = State st'
        where
            st' = \(step, c) ->
                            let val = case v of
                                        Const n -> n
                                        Var x   -> step x
                            in
                                if f val
                                then let
                                        (_, st'', c') = st (step, c)
                                     in
                                        st' (st'', c')
                                else ((), step, c)

    var :: Integer -> ProgState Variable
    var x = State $ \(st, n) -> (Var n, \l -> if l == n then x else st l, n + 1)

    lit :: Integer -> Variable
    lit = Const

    ops :: (Integer -> Integer -> Integer) -> Variable -> Variable -> ProgState ()
    ops _ (Const _) _ = State $ \(st, n) -> ((), st, n)
    ops f (Var x) (Const n) = State $ \(st, c) -> ((), \l -> if x == l then f (st x) n else st l, c)
    ops f (Var x) (Var y)   = State $ \(st, c) -> ((), \l -> if x == l then f (st x) (st y) else st l, c)

    (+=) = ops (+)
    (-=) = ops (-)
    (*=) = ops (*)

    factorial :: Integer -> Integer
    factorial n = def $ do
                    i <- var n
                    result <- var 1
                    while i (>0) $ do
                        result *= i
                        i -= lit 1
                    return result