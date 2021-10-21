
data Btree a = NilBT | Node {info::a,leftson::Btree a,rightson::Btree a}
              deriving (Show,Eq,Ord)

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
          