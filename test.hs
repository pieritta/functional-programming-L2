ls :: [Int]
ls = [1,4,6,8,3,4,5,2,4,9]
lt :: [Float]
lt = [1,4,6,8,3,4,5,2,4,9]
ll = "ellie"
test :: Int->Bool
test a = if a>10 then True else False
som :: [Int] -> Int
som [] = 0
som (x:xs) = x + som xs
prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs
divi :: [Float] -> Float
divi [] = 1 
divi (x:xs) = x / divi xs
sous :: [Int] -> Int
sous [] = 0
sous (x:xs) = x - sous xs
carre :: [Int] -> [Int]
carre [] = []
carre (x:xs) = [x*x] ++ carre xs
sqr :: [Float] -> [Float]
sqr [] = []
sqr (x:xs) = [sqrt x] ++ sqr xs
l_sec :: [Int] -> Int
l_sec xs = last (init xs)
palindrome :: [Char] -> Bool
palindrome xs = if xs == reverse xs then True else False 
palin :: [Char]-> Bool
palin xs = xs == reverse xs
mini :: [Int] -> Int
mini [] = error "liste vide"
mini (x:[]) = x
mini (x:xs) = x`min` (mini xs)
maxi :: [Int] -> Int
maxi [] = error "liste vide"
maxi (x:[]) = x
maxi (x:xs) = x`max` (maxi xs)
recherche :: Int -> [Int] -> Bool
recherche a [] = False
recherche a (x:xs) = if a == x then True else recherche a xs


--def etudiants classiquue

etudiant = ("tommy"::[Char],(("math"::[Char],10::Int),("svt"::[Char],7::Int)));

--recuperation du nom de l'etudiant
nom_etud :: ([Char], (([Char], Int), ([Char], Int))) -> [Char]
nom_etud (x,((y,z),(t,v))) = x

--recuperation note
note_etud :: ([Char], (([Char], Int), ([Char], Int))) -> (([Char], Int), ([Char], Int))
note_etud (x,((y,t),(u,v))) = ((y,t),(u,v))

--recuperation premiere matiere
note_fst :: ([Char], (([Char], Int), ([Char], Int))) -> ([Char], Int)
note_fst (x,((y,t),(u,v))) = fst (note_etud (x,((y,t),(u,v))))

fusion :: [Char] -> [Float] -> [(Char,Float)]
fusion [] [] = []
fusion [] xs = []
fusion xs [] = []
fusion (x:xs) (y:ys) = [(x,y)] ++ (fusion xs ys)

{-
fusion3 :: [Char] -> [Float] -> [Int] -> [(Char,Float,Int)]
fusion3 [] [] [] = []
fusion3 [] [] xs = []
fusion3 [] xs [] = []
fusion3 xs [] [] = []
fusion3 xs  [] = []
fusion3 (x:xs) (y:ys) = [(x,y)] ++ (fusion3 xs ys)

fuss :: [Char] -> [Float] -> [Int] -> [(Char,Float,Int)]
fuss [] [] [] = []
fuss [] [] xs = []
fuss [] xs [] = []
fuss xs [] [] = []
fuss xs  [] = []
fuss (x:xs) (y:ys) = [(x,y)] ++ (fuss xs ys)

 les types de base , instance et al -}

data Unite = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

--instance pour l'affichage
instance Show Unite where
  show Lundi = "Lundi"
  show Mardi = "Mardi"
  show Mercredi = "Mercredi"
  show Jeudi = "Jeudi"
  show Vendredi = "Vendredi"
  show Samedi = "Samedi"
  show Dimanche = "Dimanche"  

instance Eq Unite where
  (==) y x = ((show y) == (show x))
  (/=) y x = not $ y == x

instance Ord Unite where
  min Lundi _ = Lundi
  min _ Lundi = Lundi
  min Mardi x = if x /= Lundi then Mardi else Lundi
  min x Mardi = if x /= Lundi then Mardi else Lundi
  min Mercredi x = if x /= Mardi && x /= Lundi then Mercredi else x
  min x Mercredi = if x /= Mardi && x /= Lundi then Mercredi else x
  min Jeudi x = if x /= Lundi && x /= Mardi && x /= Mercredi then Jeudi else x
  min x Jeudi = if x /= Lundi && x /= Mardi && x /= Mercredi then Jeudi else x
  min Vendredi x = if x /= Lundi && x /= Mardi && x /= Mercredi && x /= Jeudi then Vendredi else x
  min x Vendredi = if x /= Lundi && x /= Mardi && x /= Mercredi && x /= Jeudi then Vendredi else x
  min Samedi x = if x == Samedi || x == Dimanche then Samedi else x
  min x Samedi = if x == Samedi || x == Dimanche then Samedi else x
  min Dimanche x = if x == Dimanche then Dimanche else x
  min x Dimanche = if x == Dimanche then Dimanche else x


instance Enum Unite where
  succ Lundi = Mardi
  succ Mardi = Mercredi
  succ Mercredi = Jeudi
  succ Jeudi = Vendredi
  succ Vendredi =Samedi
  succ Samedi = Dimanche
  succ Dimanche = Lundi

  pred Lundi = Dimanche
  pred Mardi = Lundi
  pred Mercredi = Mardi
  pred Jeudi = Mercredi
  pred Vendredi = Jeudi
  pred Samedi = Vendredi
  pred Dimanche = Samedi

lss = [Lundi , Mardi , Jeudi , Lundi , Dimanche , Mercredi , Jeudi]

succes :: [Unite] -> [Unite]
succes [] = []
succes (x:xs) = [succ x] ++ succes xs 

