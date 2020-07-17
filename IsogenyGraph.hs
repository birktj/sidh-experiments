module IsogenyGraph where

import Data.Graph
import Data.List
import Fp2
import Montgomery
import Isogeny

isogeny2paths :: (Eq a, Fractional a) => Integer -> MontPoint a -> MontPoint a -> [[MontPoint a]]
isogeny2paths n p q = [reverse . path2isogeny . montAdd p $ montMul k q | k <- [0..2^n - 1]]

isogeny2graph :: (Eq a, Ord a, Fractional a) => Integer -> MontPoint a -> MontPoint a -> Tree a
isogeny2graph n p q = head .  helper . sort
                    . map (\c -> map jInvariant $ map montCurve c ++ [curve2isogeny $ last c])
                    $ isogeny2paths n p q
    where
        helper = map (\xs -> Node (head $ head xs) . helper . filter (not . null) $ map tail xs)
               . groupBy (\a b -> head a == head b) 

isogeny3paths :: (Eq a, Fractional a) => Integer -> MontPoint a -> MontPoint a -> [[MontPoint a]]
isogeny3paths n p q = [reverse . path3isogeny . montAdd p $ montMul k q | k <- [0..2^n - 1]]

isogeny3graph :: (Eq a, Ord a, Fractional a) => Integer -> MontPoint a -> MontPoint a -> Tree a
isogeny3graph n p q = head .  helper . sort
                    . map (\c -> map jInvariant $ map montCurve c ++ [curve3isogeny $ last c])
                    $ isogeny3paths n q p
    where
        helper = map (\xs -> Node (head $ head xs) . helper . filter (not . null) $ map tail xs)
               . groupBy (\a b -> head a == head b) 

showGraphDot :: Tree (Fp2 n) -> String
showGraphDot = unlines . helper
    where
        helper (Node x cs) = map (printLine x) cs ++ concat (map helper cs)
        printLine x (Node y _) = "\"" ++ printF2 x ++ "\" -> \"" ++ printF2 y ++ "\""
        printF2 (Fp2 a 0) = show a
        printF2 (Fp2 0 b) = show b ++ "i"
        printF2 (Fp2 a b) = show a ++ " + " ++ show b ++ "i"

