

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xs) = (x:premier,y:second)
          where
          premier = fst (unzip' xs)
          second = snd (unzip' xs)

unzip'' :: [(a,b)] -> ([a],[b])
unzip'' [] = ([],[])
unzip'' ((x,y):xs) = (x:premier,y:second)
          where
          premier = map fst xs
          second = map snd xs


data Vector a = Vector a a a deriving (Show)

vectplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vectplus` (Vector a b c) = Vector (i+a) (j+b) (k+c)


data Person = Person { firstName :: String
, lastName :: String
, age :: Int
} deriving (Eq,Show,Read)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook


retire ::(Eq b,Num b)=> [a] -> b -> Maybe a
retire [] n = Nothing
retire (x:xs) n
         | n == 0 = Just x
         | otherwise = retire xs (n-1)


type Triplet x y z = (Int,Int,Bool)

type TripletPoly a b c = (a,b,c)

first :: Triplet a b c -> Int
first (q,_,_) = q

fist :: TripletPoly a b c -> a
fist (x,_,_) = x

data MyNum  = Myint Int  | Myfloat Float 
             deriving (Show)

sq :: MyNum -> MyNum
sq (Myint x) = Myint (x*x)
sq (Myfloat a) = Myfloat (a*a)

data List a = Nil | Cons {headlist::a , taillist::List a}
            deriving (Eq,Ord)


instance  (Show z) =>Show (List z) where
  show Nil = "Nil"
  show (Cons x Nil) = "Cons " ++ show x ++ " Nil"
  show (Cons y ys) = "Cons "++show y ++"(" ++ show ys ++")" 

flatten :: [[a]] -> [a]
flatten [[]]       = []
flatten (xs:xss) = xs ++ flatten xss

-- CEST CA ICI
map'::(t->t1)->List t->List t1
map' f Nil = Nil
map' f (Cons a as) = f a `Cons` (map' f as)

swapall::List (a,b) ->List (b,a)
swapall = map'(\(x,y)->(y,x))

plus'::List t->List t->List t
plus' Nil (Cons a as) = (Cons a as)
plus'(Cons x xs) ys = x`Cons`fromlist((tolist xs) ++ (tolist ys)) 

plu::List t->List t->List t
plu Nil Nil = Nil
plu Nil ys = ys
plu (Cons x xs) ys = x`Cons`plu xs ys

flatten''::List (List t) ->List t
flatten'' Nil = Nil
flatten'' (Cons xs xss) = xs `plu`flatten'' xss

fromlist :: [a] -> List a
fromlist [] = Nil
fromlist (x:xs) = x`Cons`fromlist xs 

tolist::List a -> [a]
tolist Nil = []
tolist(Cons a as) = a:tolist as

-- mapfla::list(list (list t)) -> list (list t)

-- CA FINIT ICI








swapall'::List (a,b) ->List (b,a)
swapall' Nil =Nil
swapall'(Cons (a,b) ys) = (b,a) `Cons`swapall' ys

alist::List (List Integer)
alist = let 
          l = fromlist [4,5,3,2] 
          t = Cons 8(Cons 2 Nil)  
         in  fromlist [t,l]


plus :: [a] -> [a] -> [a]
plus [] ys = ys
plus (x:xs) ys = x:(xs ++ ys)


data List' a = Vide | Ajoute a (List' a)
              deriving(Eq,Ord,Show)



data Nombre = Nb {partreel::Float, partim::Float}
            deriving (Eq)
instance Show Nombre where
     show (Nb a b) = "("++show a ++","++"i"++show b++")"


instance Ord Nombre where
  (Nb a b) >= (Nb c d) = sqrt ((a*a)+(b*b)) >= sqrt ((c*c)+(d*d))
  (Nb a b) <= (Nb c d) = sqrt ((a*a)+(b*b)) <= sqrt ((c*c)+(d*d))
  (Nb a b) > (Nb c d) = not ((Nb a b) <= (Nb c d))


somme' :: Nombre -> Nombre -> Nombre
somme' (Nb {partreel=a, partim=b}) (Nb c d) = (Nb (a+c) (b+d))

type Complexe =  (Float,Float)

somme'' ::Complexe->Complexe->Complexe
somme'' (r1,i1) (r2,i2) = (r3,i3)
                        where
                        r3 = r1+r2
                        i3=i1+i2

type Point = (Float,Float) 

data Figure = Cercle Float Float Float| Rectangle Float Float Float Float| Carre Float
                        deriving (Eq,Show,Ord)


triinsert::(Ord a)=>[a] ->[a]
triinsert [] = []
triinsert [a] = [a]
triinsert (x:xs) = insert x (triinsert xs)
                 where 
                 insert a [] =[a]
                 insert a (x:xs) 
                  | a < x = a:x:xs
                  |otherwise = x:insert a xs
data Btree a = NilBT | Node {info::a,leftson::Btree a,rightson::Btree a}
              deriving (Show,Eq,Ord)


class Figuregeometrique a where
  surface::a-> Float
  perimetre::a -> Float
instance Figuregeometrique (Figure) where
 surface (Cercle a b c) = c*c 

so::(Num a)=>Btree a -> a
so NilBT = 0
so (Node a left right)=a + (so (left)) + (so (right))

prefixe:: Btree a -> [a]
prefixe NilBT = []
prefixe (Node e g d) = [e]++ prefixe g  ++ prefixe d

infixe:: Btree a -> [a]
infixe NilBT = []
infixe (Node a NilBT NilBT) = [a]
infixe (Node a NilBT y) = infixe y ++ [a]
infixe (Node x y  NilBT) = infixe y ++[x]
infixe (Node e f k) = infixe f ++ [e] ++ infixe k

postfixe:: Btree a -> [a]
postfixe NilBT = []
postfixe (Node a NilBT NilBT) = [a]
postfixe (Node a b NilBT) = postfixe b ++ [a]
postfixe (Node a NilBT b) = postfixe b ++[a]
postfixe (Node e (Node g z f) (Node d i k)) = postfixe z ++ postfixe f ++ [g] ++  postfixe i ++postfixe k ++ [d] ++ [e]

profondeur::Btree a -> [a]
profondeur NilBT = []
profondeur (Node a NilBT NilBT) = [a]
profondeur (Node a b NilBT) = [a] ++ profondeur b
profondeur (Node a NilBT b) = [a] ++ profondeur b
profondeur (Node a b c) = [a] ++ profondeur b ++ profondeur c 

mapTree::(a -> b) -> Btree a -> [b]
mapTree f NilBT = []
mapTree f (Node a b c) = [f a] ++ mapTree f b ++ mapTree f c


recherche::(Eq a,Ord a)=>a->Btree a-> Bool
recherche _ NilBT = False
recherche e (Node a (Node x y z) (Node q s d))
             |e==a = True
             | e < x = recherche e y
             | e>q = recherche e d



reverse' :: [a]->[a]
reverse' (x:xs) = foldl (\acc x -> x:acc) [x] xs

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x:take' (n-1) xs

take'' :: Int -> [a] -> [a]
take'' n (x:xs)
      |n <= 0 = []
      |otherwise = x:take'' (n-1) xs

data Li t = Chaine{tete::t,listeresiduel::Li t} | Vi
           deriving(Show)
data T a = C a |R [T a]

eleindice::(Eq a)=>a->[a]->[Int]
eleindice _ [] = []
eleindice a [x] = if a == x then [0] else []
eleindice a (x:xs) 
          |a==x = (length (reverse xs) +2):eleindice a xs
          | otherwise =  eleindice a xs
          