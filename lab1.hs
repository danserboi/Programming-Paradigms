import Data.List
--1
f1 x = 5
--2
g x y = x
--3
h :: Bool -> Bool -> Bool
h True True = True
h _ _ = False
--5
myIf :: Bool -> a -> a -> a
myIf True x y = x
myIf False x y = y
--6
{-
if(x > y)
	if(z > x)
		ret z
	else
		ret x
else
	if(z > y)
		ret z
	else
		ret y
-}
maxim :: Int -> Int -> Int -> Int
maxim x y z = (myIf (z>(myIf (x>y) x y)) z (myIf (x>y) x y))
--7
f2 x y z = if x then y else z
f3 x y z
  | x == True = y
  | otherwise = z
--8
maxim2 x y z
  | ((x > y) && (x > z)) = x
  | ((y > x) && (y > z)) = y
  | otherwise = z
--10
rev :: [Integer] -> [Integer] -> [Integer]
rev [] y = y
rev x y = rev (tail x) ((head x):y)
--11
maxim3 x y z = (head (rev (sort (x:(y:(z:[])))) []))
--12
antepen x 
  | mod (head (tail (tail (rev x [])))) 2 == 1 = True
  | otherwise = False
--13
mySum l = let sumAux [] acc = acc
              sumAux (x:xs) acc = sumAux xs (x + acc)
          in sumAux l 0
--14
falseBool (x:xs)
  | x == False = False
  | xs == [] = True
  | otherwise = (falseBool xs)
--15
evenList :: [Integer] -> [Integer]
evenList list = auxEvenList list []
                   where
                        auxEvenList :: [Integer] -> [Integer] -> [Integer]
                        auxEvenList [] acc = reverse acc
                        auxEvenList l1 l2
                           | (head l1 `mod` 2 == 0) = auxEvenList (tail l1) (head l1 : l2)
                           | otherwise = auxEvenList (tail l1) l2
evenListV2 :: [Integer] -> [Integer]
evenListV2 [] = []
evenListV2 (x:list) = if x `mod` 2 == 0 then x:(evenListV2 list) else (evenListV2 list)
--16
b :: [Bool] -> [Int]
b [] = []
b l = (g (head l)):(b (tail l))
    where
        g True = 1
        g False = 0
--17
f :: [[Integer]] -> [Bool]
f [] = []
f l = (g (head l)):(f (tail l))
    where
        g [] = True
        g l = h (tail l)
        h [] = True
        h l = False
--18
noTrue :: [Bool] -> Int
noTrue [] = 0
noTrue (x:list) = if x == True then (1 + (noTrue list)) else (noTrue list)
--19
myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (y:ys) = if x < y 
                 then x:y:ys 
         else y : myInsert x ys
insertionSort :: [Int] -> [Int]
insertionSort [x] = [x]
insertionSort (x:xs) = myInsert x (insertionSort xs)

--din laboratorul vechi 1
myFunc :: Int -> Int -> Int
myFunc x y = abs(2 * max x y)
{-
 - if este o expresie; cum toate expresiile
 - trebuie sa intoarca o valoare, ramura else
 - este necesara
 -
 - exista deja o functie "sum" predefinita si
 - vrem sa evitam ambiguitatea apelului, motiv
 - pentru care folosim un nume nou.
 - (e interesat de mentionat ca, in Haskell,
 - apostroful este un caracter valid in numele
 - unei functii, i.e. am putea defini o functie
 - sum'.
 -}
mySum1 l = if null l
              then 0
              else head l + mySum1 (tail l)
mySum2 [] = 0
mySum2 (x:xs) = x + mySum2 xs

mySum3 l = let sumAux [] acc = acc
               sumAux (x:xs) acc = sumAux xs (x + acc)
           in sumAux l 0
