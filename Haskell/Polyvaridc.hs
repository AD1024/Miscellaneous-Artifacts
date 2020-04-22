{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExplicitForAll #-}
module Polyvaridic where
    class PolyWord r where
        polyWords' :: String -> r

    instance PolyWord String where
        polyWords' = id

    instance (PolyWord r) => PolyWord (String -> r) where
        polyWords' f s = polyWords' (if f /= "" then f ++ " " ++ s else s)

    polyWords :: forall r. (PolyWord r) => r
    polyWords = polyWords' ""

    class PolyAdd r where
        polyAdd' :: Integral a => a -> r
    
    instance PolyAdd Int where
        polyAdd' x = fromIntegral x
 
    instance (PolyAdd r, Integral a) => PolyAdd (a -> r) where
        polyAdd' x y = polyAdd' (x + (fromIntegral y))

    polyAdd :: forall r. (PolyAdd r) => r
    polyAdd = polyAdd' (0 :: Int)

    class PolyList a r | r -> a where
        polyList' :: [a] -> a -> r
        identity :: [a] -> r

    instance PolyList a [a] where
        polyList' l x = l ++ [x]
        identity = id 

    instance PolyList a r => PolyList a (a -> r) where
        polyList' l x y = polyList'(l ++ [x]) y
        identity l = polyList' l

    polyList :: forall r a. (PolyList a r) => r
    polyList = identity []