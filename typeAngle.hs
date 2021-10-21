type Angle = Float
type Position = (Float,Float)
type Pairs a = (a,a)
type Automorphisme a = a->a
type Flag a = (a,Bool)
 data Angle = MKAngle Float
 instance Eq Angle where
 	MKAngle x == MKAngle y = normalise x == normalise y
 	normalise::Float->Float
 	normalise x
 	   | x < 0 = normalise (x+rot)
 	   | x >= rot = normalise (x-rot)
 	   | otherwise = x
 	   where rot = 2*3.14159 
 	   newtype Angle = MKAngle Float