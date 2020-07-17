{-# LANGUAGE DataKinds, KindSignatures #-}

module Fp2 where

import GHC.TypeNats (Nat, KnownNat, natVal)
import Data.Ratio

data Fp2 (n :: Nat) = Fp2 Integer Integer
    deriving (Eq, Ord)

instance KnownNat n => Show (Fp2 n) where
    show mx@(Fp2 a b)
        | b == 0    = "(" ++ show a ++ " :: Fp2 " ++ show n ++ ")"
        | otherwise = "(" ++ show a ++ " + " ++ show b ++ "*i :: Fp2 " ++ show n ++ ")"
        where
            n = toInteger $ natVal mx

instance KnownNat n => Num (Fp2 n) where
    mx@(Fp2 a b) + (Fp2 c d) = Fp2 (mod (a + c) n) (mod (b + d) n)
        where
            n = fromIntegral $ natVal mx
    mx@(Fp2 a b) - (Fp2 c d) = Fp2 (mod (a - c) n) (mod (b - d) n)
        where
            n = fromIntegral $ natVal mx
    mx@(Fp2 a b) * (Fp2 c d) = Fp2 (mod (a * c - b * d) n) (mod (a * d + b * c) n)
        where
            n = fromIntegral $ natVal mx
    fromInteger i = mx
        where
            mx = Fp2 (mod i . fromIntegral $ natVal mx) 0
    abs = undefined
    signum = undefined

instance KnownNat n => Fractional (Fp2 n) where
    a / b = a * inv b
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = (g, t - div b a * s, s)
    where
        (g, s, t) = egcd (mod b a) a
    
inv :: KnownNat n => Fp2 n -> Fp2 n
inv mx@(Fp2 a b) = Fp2 (mod (a * d) n) (mod (- b * d) n)
    where
        (_, d, _) = egcd (mod (a^2 + b^2) n) n
        n = fromIntegral $ natVal mx

i :: KnownNat n => Fp2 n
i = Fp2 0 1
