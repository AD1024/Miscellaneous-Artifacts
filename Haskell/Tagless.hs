{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}

module Tagless where
    import Control.Applicative
    import Control.Monad
    import Control.Monad.Writer hiding (fix)
    import Data.Functor.Identity

    class Symantics repr where
        int  :: Int -> repr Int
        bool :: Bool -> repr Bool
        add  :: repr Int -> repr Int -> repr Int
        mult :: repr Int -> repr Int -> repr Int
        leq  :: repr Int -> repr Int -> repr Bool
        equiv :: repr Int -> repr Int -> repr Bool

        lam  :: (repr a -> repr b) -> repr (a -> b)
        app  :: repr (a -> b) -> repr a -> repr b
        fix  :: (repr a -> repr a) -> repr a

        ite  :: repr Bool -> repr a -> repr a -> repr a

    factorial :: Symantics repr => () -> repr (Int -> Int)
    factorial () = fix (\f -> lam (\x -> ite (equiv x (int 0)) 
                                            (int 1) 
                                            (mult x 
                                                (app f (add x (int (-1)))))))
    type Eval = Identity

    type Printer = Writer String

    runEval :: Eval a -> a
    runEval = runIdentity

    runPrinter :: Printer a -> (a, String)
    runPrinter = runWriter

    instance Symantics Eval where
        int = return
        bool = return
        add = liftA2 (+)
        mult = liftA2 (*)
        leq = liftA2 (<=)
        equiv = (liftA2) (==)
        lam f = return (runEval . f . return)
        app = ap
        fix f = f (fix f)

        ite cond ib tb = do
                            c <- cond
                            if c then ib else tb
    
    instance Symantics Printer where
        int x = do
                    tell $ " Int (" ++ show x ++ ") "
                    return x
        bool x = do
                    tell $ " Bool (" ++ show x ++ ") "
                    return x
        add x y = do
                    tell "Add ("
                    e1 <- x
                    tell ", "
                    e2 <- y
                    tell ")"
                    return (e1 + e2)
        leq x y = do
                    do
                    tell "Leq? ("
                    e1 <- x
                    tell ", "
                    e2 <- y
                    tell ")"
                    return (e1 <= e2)
        
        equiv x y = do
                    tell "Eq? ("
                    e1 <- x
                    tell ", "
                    e2 <- y
                    tell ")"
                    return (e1 == e2)

        mult x y = do
                    tell "Mult ("
                    e1 <- x
                    tell ", "
                    e2 <- y
                    tell ")"
                    return (e1 * e2)

        lam f = do
                    tell $ " Lambda () "
                    return (fst . runWriter . f . return)
        
        app f x = do
                tell "App ("
                func <- f
                p <- x
                tell ")"
                return (func p)
        
        fix f = f (fix f)
    
        ite cond ib eb = do
                            tell "If ("
                            c <- cond
                            tell ") \n then ("
                            ibv <- ib
                            tell ") \n else ("
                            ebv <- eb
                            tell ")"
                            return (if c then ibv else ebv)


    evalFact :: () -> Eval (Int -> Int)
    evalFact () = fix (\f -> lam (\x -> ite (equiv x (int 0)) 
                                            (int 1) 
                                            (mult x 
                                                (app f (add x (int (-1)))))))
    
    printExp :: () -> Printer Int
    printExp () = add (mult (int 2) (int 3)) (int 5)

    printLam :: () -> Printer (Int -> Int)
    printLam () = lam (\x -> add (int 1) x)

    -- ill-formed
    -- ill = add (int 0) (lam (\x -> x))