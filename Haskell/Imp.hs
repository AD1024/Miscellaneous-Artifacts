module Imp where
    
    import Prelude hiding (lookup)
    import Control.Monad.State
    import Data.Map
    import Data.Maybe

    data Var = Var Integer | Const Integer

    data Heap = Heap {
        variables :: Map Integer Integer,
        varCount  :: Integer
    }

    type ProgState a = State Heap a

    def :: ProgState Var -> Integer
    def st = case (runState st) (Heap empty 0) of
                (Var x, heap) -> fromJust $ lookup x (variables heap)
                (Const x, _)  -> x

    var :: Integer -> ProgState Var
    var i = do
                progstate <- get
                let heap = variables progstate
                let vcount = varCount progstate
                put (Heap (insert (vcount + 1) i heap) (succ vcount))
                return (Var (vcount + 1))
    
    while :: Var -> (Integer -> Bool) -> ProgState () -> ProgState ()
    while (Var x) f st = do
                        progstate <- get
                        let heap = variables progstate
                        let val = fromJust $ lookup x heap
                        if f val then st >> while (Var x) f st else return ()
    while _ _ _ = undefined

    lit :: Integer -> Var
    lit = Const

    ops :: (Integer -> Integer -> Integer) -> Var -> Var -> ProgState ()
    ops f (Var x) (Const i) =
        do
            progstate <- get
            let heap = (variables progstate)
            let v = fromJust $ lookup x heap
            put (Heap (update (Just . const (f v i)) x heap) (varCount progstate))
    ops f (Var x) (Var y) =
        do
            progstate <- get
            let heap = (variables progstate)
            let v1 = fromJust $ lookup x heap
            let v2 = fromJust $ lookup y heap
            put (Heap (update (Just . const (f v1 v2)) x heap) (varCount progstate))
    ops _ (Const _) _ = do return ()

    (+=) = ops (+)
    (-=) = ops (-)
    (*=) = ops (*)

    -- factorial :: Integer -> Integer
    -- factorial n = def $ do
    --                     result <- var 1
    --                     i      <- var n
    --                     while i (>0) $ do
    --                         result *= i
    --                         i      -= lit 1
    --                     return result