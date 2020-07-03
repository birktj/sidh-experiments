module Montgomery where

data MontCurve a = MontCurve a a 
    deriving (Eq, Show)

jInvariant :: Fractional a => MontCurve a -> a
jInvariant (MontCurve a _) = 256*(a^2 - 3)^3 / (a^2 - 4)

data MontPoint a = MontPoint (MontCurve a) a a | MontInf (MontCurve a)
    deriving (Eq, Show)

montPoint :: (Eq a, Fractional a) => MontCurve a -> a -> a -> MontPoint a
montPoint curve x y 
    | montPointIsValid p = p
    | otherwise          = error "Point is not on the curve"
    where
        p = MontPoint curve x y

montCurve :: MontPoint a -> MontCurve a
montCurve (MontPoint c _ _) = c
montCurve (MontInf c)       = c

montAdd :: (Eq a, Fractional a) => MontPoint a -> MontPoint a -> MontPoint a
montAdd p1@(MontPoint c@(MontCurve a b) x1 y1) p2@(MontPoint c' x2 y2)
    | c /= c'              = error "Can only add equal curves"
    | x1 == x2 && y1 == y2 = montDouble p1
    | x1 == x2             = MontInf c
    | otherwise            = montPoint c x3 y3
    where
        l = (y2 - y1) / (x2 - x1)
        x3 = b*l^2 - a - x1 - x2
        y3 = l*(x2 - x3) - y2
montAdd p (MontInf _) = p
montAdd (MontInf _) p = p

montDouble :: (Eq a, Fractional a) => MontPoint a -> MontPoint a
montDouble (MontPoint c@(MontCurve a _) x y)
    | y == 0 = MontInf c
    | otherwise = montPoint c x' y'
    where
        x' = (x^2 - 1)^2 / (4*x*(x^2 + a*x + 1))
        y' = y*(x^2 - 1)*(x^4 + 2*a*x^3 + 6*x^2 + 2*a*x + 1)
           / (8*x^2*(x^2 + a*x + 1)^2) 
montDouble p@(MontInf _) = p

montMul :: (Eq a, Fractional a) => Integer -> MontPoint a -> MontPoint a
montMul 0 x = montId (montCurve x)
montMul n x
    | even n    = montDouble x'
    | otherwise = montAdd x $ montDouble x'
    where
        x' = montMul (div n 2) x

montPointIsValid :: (Eq a, Fractional a) => MontPoint a -> Bool
montPointIsValid (MontPoint (MontCurve a b) x y) = b*y^2 == x^3 + a*x^2 + x

montId :: MontCurve a -> MontPoint a
montId c = MontInf c

montOrder :: (Eq a, Fractional a) => MontPoint a -> Integer
montOrder p = fst . head . filter ((==p) . snd) . tail . zip [0..] $ iterate (montAdd p) p
