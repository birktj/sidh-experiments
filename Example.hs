{-# LANGUAGE DataKinds #-}

module Example where

import Fp2
import Montgomery
import Isogeny
import IsogenyGraph
import Sidh

-- Public parameters
a0 :: Fp2 431
a0 = 329*i + 423

ea0 :: MontCurve (Fp2 431)
ea0 = MontCurve a0 1

shared_data = SharedData {
        qa = montPoint ea0 (100*i + 248) (304*i + 199),
        pa = montPoint ea0 (426*i + 394) (51*i + 79),
        pb = montPoint ea0 (358*i + 275) (410*i + 104),
        qb = montPoint ea0  (20*i + 185) (281*i + 239)
    }

-- Alice secret 
ka = 11

-- Bob secret 
kb = 2

-- Alice public key 
pk_a = genPublicKey Alice shared_data ka


-- Bob public key
pk_b = genPublicKey Bob shared_data kb


-- Alice shared secret
sk_a = genSharedKey Alice pk_b ka

-- Bob shared secret
sk_b = genSharedKey Bob pk_a kb

iso_g0 = isogeny2graph 5 (pa shared_data) (qa shared_data)


ea1 :: MontCurve (Fp2 863)
ea1 = MontCurve 0 1

p1 = montPoint ea1 (131*i + 1) (752*i + 223)
q1 = montPoint ea1 (18*i + 223) (620*i + 112)

iso_g1 = isogeny2graph 8 p1 q1
