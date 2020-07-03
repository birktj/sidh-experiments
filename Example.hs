{-# LANGUAGE DataKinds #-}

module Example where

import Fp2
import Montgomery
import Isogeny

-- Public parameters
a0 :: Fp2 431
a0 = 329*i + 423

ea0 :: MontCurve (Fp2 431)
ea0 = MontCurve a0 1

pa = montPoint ea0 (100*i + 248) (304*i + 199)
qa = montPoint ea0 (426*i + 394) (51*i + 79)

pb = montPoint ea0 (358*i + 275) (410*i + 104)
qb = montPoint ea0  (20*i + 185) (281*i + 239)

-- Alice secret 
ka = 3

-- Bob secret 
kb = 2

-- Alice public key generation
sa = montAdd pa $ montMul ka qa

iso_a = combine2isogeny $ path2isogeny sa

path_a = (map (jInvariant . curve2isogeny) $ path2isogeny sa) ++ [jInvariant ea0]

-- Alice public key
pb_a = iso_a pb
qb_a = iso_a qb


-- Bob public key generation
sb = montAdd pb $ montMul kb qb

iso_b = combine3isogeny $ path3isogeny sb

path_b = (map (jInvariant . curve3isogeny) $ path3isogeny sb) ++ [jInvariant ea0]

-- Bob public key
pa_b = iso_b pa
qa_b = iso_b qa


-- Alice shared secret generation
sa' = montAdd pa_b $ montMul ka qa_b
priv_a = jInvariant . curve2isogeny . head $ path2isogeny sa'

-- Bob shared secret generation
sb' = montAdd pb_a $ montMul kb qb_a
priv_b = jInvariant . curve3isogeny . head $ path3isogeny sb'
