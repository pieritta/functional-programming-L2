data Jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche deriving(Eq,Ord,Enum)
class Enum a where
	toEnum :: a->Int
	fromEnum :: Int->a

	instance Enum jour where
	 	toEnum dimanche = 0
	 	toEnum lundi = 1
	 	toEnum mardi = 2
	 	toEnum mercredi = 3
	 	toEnum jeudi = 4
	 	toEnum vendredi =  5
	 	toEnum samedi = 6

	 instance Eq jour where
	 	(x==y) = (toEnum x == toEnum y)
	 instance Ord jour where
	 	(x<y) = (toEnum x < toEnum y) 