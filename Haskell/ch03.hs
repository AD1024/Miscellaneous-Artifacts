import Data.List

data BTree a = Leaf a | Empty | Branch (BTree a) (BTree a) deriving (Eq, Show)

computeLength :: [a] -> Int
computeLength [] = 0
computeLength (x : xs) = 1 + (computeLength xs)

mean :: [Int] -> Float
mean [] = 0
mean xs = (fromIntegral (calcSum xs)) / (fromIntegral $ computeLength xs)
    where
        calcSum :: [Int] -> Int
        calcSum [] = 0
        calcSum (x : xs) =  x  + (calcSum xs)

pailndrome :: [a] -> [a]
pailndrome [] = []
pailndrome xs = xs ++ (rev xs)
    where
        rev :: [a] -> [a]
        rev [] = []
        rev (x : xs) = rev xs ++ [x]

isPailndrome :: Eq a => [a] -> Bool
isPailndrome [] = True
isPailndrome xs
    | (computeLength xs) `mod` 2 == 1 = False
    | otherwise = let halfLength = (computeLength xs) `div` 2
                  in
                    (take halfLength xs) == reverse (drop halfLength xs)

sortLength :: [[a]] -> [[a]]
sortLength [] = []
sortLength xs = sortBy compLength xs
    where
        compLength :: [a] -> [a] -> Ordering
        compLength a b
            | computeLength a > computeLength b = GT
            | computeLength a < computeLength b = LT
            | otherwise                   = EQ

intersperseMe :: a -> [[a]] -> [a]
intersperseMe _ [] = []
intersperseMe sep (x : xs) = (jojo sep x) ++ intersperseMe sep xs
    where
        jojo :: a -> [a] -> [a]
        jojo _ [] = []
        jojo _ (x : []) = [x]
        jojo sep (x : xs) = [x, sep] ++ jojo sep xs

heightOf :: BTree a -> Int
heightOf Empty = 0
heightOf (Leaf _) = 1
heightOf (Branch lchild rchild) = max (heightOf lchild) (heightOf rchild)

type Point = (Int, Int)
type Vector = (Int, Int)

data Directions = Left | Right | Line

vectorize :: Point -> Point -> Vector
vectorize (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- decideTurn :: Point -> Point -> Point -> Direction
-- decideTurn a b c = let
--                         vec1 = vectorize a b
--                         vec2 = vectorize b c
--                     in