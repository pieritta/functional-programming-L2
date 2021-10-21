data Poly = Pvide | Monome Float Float  Poly
addPoly::Poly->Poly->Poly
addPoly p Pvide = p
addPoly Pvide p = p
addPoly p1@(Monome c1 d1 p11) p2@(Monome c2 d2 p21)
       | d1==d2 = Monome(c1+c2)d1(addPoly p11 p21)
       | d1<d2 = Monome c1 d1 (addPoly p11 p21)
       | otherwise = Monome c2 d2 (addPoly p11 p21)
