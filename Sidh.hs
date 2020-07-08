module Sidh where

import Montgomery
import Isogeny

data Participant = Alice | Bob
    deriving (Eq, Show)

data SharedData a = SharedData { pa :: MontPoint a
                               , qa :: MontPoint a
                               , pb :: MontPoint a
                               , qb :: MontPoint a }

genPublicKey :: (Fractional a, Eq a)
             => Participant -> SharedData a -> Integer -> (MontPoint a, MontPoint a)
genPublicKey Alice sd k = (iso $ pb sd, iso $ qb sd)
    where
        sa = montAdd (pa sd) . montMul k $ qa sd
        iso = combine2isogeny $ path2isogeny sa
genPublicKey Bob sd k = (iso $ pa sd, iso $ qa sd)
    where
        sb = montAdd (pb sd) . montMul k $ qb sd
        iso = combine3isogeny $ path3isogeny sb

genSharedKey :: (Fractional a, Eq a)
             => Participant -> (MontPoint a, MontPoint a) -> Integer -> a
genSharedKey Alice (pa, qa) k = jInvariant . curve2isogeny . head . path2isogeny
                              . montAdd pa $ montMul k qa
genSharedKey Bob (pb, qb) k   = jInvariant . curve3isogeny . head . path3isogeny
                              . montAdd pb $ montMul k qb
