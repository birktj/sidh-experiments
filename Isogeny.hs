module Isogeny where

import Montgomery

curve2isogeny :: (Eq a, Fractional a) => MontPoint a -> MontCurve a
curve2isogeny (MontInf _) = error "Point must be of order 2"
curve2isogeny p@(MontPoint (MontCurve a b) alpha _)
    -- | alpha == 0       = error "Alpha cannot be 0"
    | montOrder p /= 2 = error "Point must be of order 2"
    | otherwise = MontCurve a' b'
    where
        a' = 2*(1 - 2*alpha^2)
        b' = alpha*b

point2isogeny :: (Eq a, Fractional a) => MontPoint a -> MontPoint a -> MontPoint a
point2isogeny (MontInf _) _ = error "Point must be of order 2"
point2isogeny p@(MontPoint c alpha _) (MontPoint c' x y)
    -- | alpha == 0       = error "Alpha cannot be 0"
    | montOrder p /= 2 = error "Point must be of order 2"
    | c /= c'   = error "Points must have same curve"
    | otherwise = MontPoint (curve2isogeny p) x' y'
    where
        x' = x*(alpha*x - 1) / (x - alpha)
        y' = y*(x^2*alpha - 2*x*alpha^2+alpha) / (x - alpha)^2

find2Nisogeny :: (Eq a, Fractional a) => Integer -> MontPoint a -> MontPoint a
find2Nisogeny 1 p = reduceOrder 2 p
find2Nisogeny n p = find2Nisogeny (n-1) $ point2isogeny (reduceOrder 2 p) p

findAll2Nisogeny :: (Eq a, Fractional a)
                 => Integer -> MontPoint a -> MontPoint a -> [MontPoint a]
findAll2Nisogeny n p q = [find2Nisogeny n . montAdd p $ montMul k q | k <- [0..2^n-1]]

path2isogeny :: (Eq a, Fractional a) => MontPoint a -> [MontPoint a]
path2isogeny p = helper [] p
    where
        helper ps p
            | montOrder p == 2 = p:ps
            | otherwise = let r = reduceOrder 2 p in helper (r:ps) $ point2isogeny r p

combine2isogeny :: (Eq a, Fractional a) => [MontPoint a] -> MontPoint a -> MontPoint a
combine2isogeny = foldr (.) id . map point2isogeny


curve3isogeny :: (Eq a, Fractional a) => MontPoint a -> MontCurve a
curve3isogeny (MontInf _) = error "Point must be of order 3"
curve3isogeny p@(MontPoint (MontCurve a b) alpha _)
    | montOrder p /= 3 = error "Point must be of order 3"
    | otherwise = MontCurve a' b'
    where
        a' = (a*alpha - 6*alpha^2 + 6)*alpha
        b' = b*alpha^2

point3isogeny :: (Eq a, Fractional a) => MontPoint a -> MontPoint a -> MontPoint a
point3isogeny (MontInf _) _ = error "Point must be of order 3"
point3isogeny p@(MontPoint c alpha _) (MontPoint c' x y)
    | montOrder p /= 3 = error "Point must be of order 3"
    | c /= c'   = error "Points must have same curve"
    | otherwise = MontPoint (curve3isogeny p) x' y'
    where
        x' = x*(x*alpha - 1)^2 / (x - alpha)^2
        y' = y*(x*alpha - 1)*(x^2*alpha - 3*x*alpha^2 + x + alpha) / (x - alpha)^3

path3isogeny :: (Eq a, Fractional a) => MontPoint a -> [MontPoint a]
path3isogeny p = helper [] p
    where
        helper ps p 
            | montOrder p == 3 = p:ps
            | otherwise = let r = reduceOrder 3 p in helper (r:ps) $ point3isogeny r p

combine3isogeny :: (Eq a, Fractional a) => [MontPoint a] -> MontPoint a -> MontPoint a
combine3isogeny = foldr (.) id . map point3isogeny
