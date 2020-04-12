{-# LANGUAGE RankNTypes #-}
module FinalTagless where

    import Prelude hiding (and, or)

    class Language r where
        here   :: r (a, h) a
        before :: r h a -> r (any, h) a
        lambda :: r (a, h) b -> r h (a -> b)
        apply  :: r h (a -> b) -> (r h a -> r h b)
        
        loop   :: r h (a -> a) -> r h a
        
        int    :: Int -> r h Int
        add    :: r h Int -> r h Int -> r h Int
        down   :: r h Int -> r h Int    -- \x -> x - 1
        up     :: r h Int -> r h Int    -- \x -> x + 1
        mult   :: r h Int -> r h Int -> r h Int
        gte    :: r h Int -> r h Int -> r h Bool -- greater than or equal
        
        bool   :: Bool -> r h Bool
        and    :: r h Bool -> r h Bool -> r h Bool
        or     :: r h Bool -> r h Bool -> r h Bool
        neg    :: r h Bool -> r h Bool
        
        ifte   :: r h Bool -> r h a -> r h a -> r h a -- if true then return left term, else return right term
    
    fix :: forall a. (a -> a) -> a
    fix f = f (fix f)
    
    data Eval f a = Eval { run :: f -> a }

    lift :: a -> Eval f a
    lift x = Eval (const x)

    liftE1 :: (a -> b) -> Eval f a -> Eval f b
    liftE1 f e = Eval $ f . (run e)

    liftE2 :: (a -> b -> c) -> Eval f a -> Eval f b -> Eval f c
    liftE2 f a b = Eval $ \x -> f (run a x) (run b x)

    instance Language Eval where
        here = Eval fst
        before e = Eval $ (run e . snd)
        lambda e = Eval $ \h x -> run e (x, h)
        apply  = liftE2 ($)

        loop e = Eval $ (fix . run e)

        int = lift
        add = liftE2 (+)
        down = liftE1 (+(-1))
        up   = liftE1 (+1)
        mult = liftE2 (*)
        gte  = liftE2 (>=)

        bool = lift
        and  = liftE2 (&&)
        or   = liftE2 (||)
        neg  = liftE1 not

        ifte cond lb rb = Eval $ \h -> if run cond h then run lb h else run rb h

    type Term a = forall r h . Language r => r h a
    
    interpret :: Term a -> a
    interpret t = run t ()

    ex :: Term (Int -> Int -> Int)
    ex = lambda (lambda (add (before here) here))