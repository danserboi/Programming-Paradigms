--HIGHER-ORDER FUNCTIONS
--Function application and composition as higher-order functions
--Functions which take other functions as parameter are called higher-order. 
--43. Define the operator ($) from Lab 2 as a higher-order function. 
dollar :: (a -> b) -> a -> b
dollar f x = f x
--44. What type should functional composition have? 
--45. Define function composition. 
composition :: (b -> c) -> (a -> b) -> a -> c
composition g f x = g (f x)
--LAMBDAS
--46. Functions can be passed as arguments just like any other value value. Also, functions can be returned as parameter. In order to do so, it is convenient to define functions without naming them. This is done using lambda's. For a more detailed discussion regarding lambdas, see the lecture. The following definitions are equivalent: 
f46a x y = x + y 
f46b x = \y -> x + y
f46c = \x -> \y -> x + y
f46d = \x y -> x + y
-- 47. Consider sets represented as characteristic functions with signature s :: Integer → Bool, where s x is true if x a member in the set. Examples:

s1 1 = True
s1 2 = True
s1 _ = False
 
s2 x = x `mod` 2 == 0 
s3 _ = False

--Above, s1 is the set {1,2}

--, s2 is the set of even integers and s3 is the empty-set. 
--Write a function which tests if an element is a member of a set:

mem :: (Integer -> Bool) -> Integer -> Bool
mem f x = (f x)

--48. Define the set {2n∣n∈N} .
f48 :: Integer -> Bool
f48 1 = True
f48 x
     | x `mod` 2 == 1 = False
     | otherwise = (f48 (x`div`2))
--49. Define the set of natural numbers. 
nat :: Integer -> Bool
nat x
   | x >= 0 = True
   | otherwise = False
-- 50. Implement the intersection of two sets. Use lambdas.

intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection = \set set2 -> \x -> ((set x) && (set2 x))

--51. Write intersection in another way, (without using lambdas). 
intersection' :: (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
intersection' set set2 x = ((set x) && (set2 x))

-- 52. Write a function which takes a list of integers, and returns the set which contains them.

toSet :: [Integer] -> (Integer -> Bool)
toSet [] = \y -> False
toSet (x:xs) = \y -> (y == x) || (toSet xs y)

--SAU folosind elem

toSet2 :: [Integer] -> (Integer -> Bool)
toSet2 list = \x -> x `elem` list

-- 53. Implement a function which takes a list of sets and computes their intersection.

capList :: [Integer -> Bool] -> Integer -> Bool
capList [] = \x -> False
capList (set:sets) = \x -> (set x) || (capList sets x)

--FILTER & MAP

--54. Write a function which receives a predicate p :: Integer → Bool,
--a list of integers l, and returns a list of integers from l for which g is true.
--Your implementation is the filter function from Haskell.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter g [] = []
myFilter g (x:xs)
    | ((g x) == True) = (x:(myFilter g xs))
    | otherwise = (myFilter g xs)
--55. Solve exercise 24. from Lab 2 using filter. 
filterNeg :: [Integer] -> [Integer]
filterNeg l = filter poz l where poz x = x > 0
--56. Test the function map::(a→b) → [a] → [b] from Haskell. Implement it. 
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = ((f x):(mymap f xs))
--57. Solve exercise 17. from Lab 1 using map. 
--17. Implement a function which takes a list of booleans and 
--returns a list of integers. 
--In the latter, (True becomes 1 and False becomes 0). 
--Example: f [False, True, False] = [0,1,0]. 
f57 :: [Bool] -> [Integer]
f57 = mymap trbool where
          trbool :: Bool -> Integer
          trbool True = 1
          trbool False = 0
--58. Solve exercise 27. from Lab 2 using map and filter. 
--(Hint. Pattern matching can be used in lambdas. 
--Use fst and snd to introspect pairs.) 

cuM :: [String] -> [String]
cuM = filter incepecuM where
                       incepecuM :: String -> Bool
                       incepecuM [] = False
                       incepecuM (x:xs) = (x == 'M')

f58 :: String -> [(String, [String])] -> [String]
f58 group [] = []
f58 group (x:xs)
       | group == (fst x) = cuM (snd x) ++ (f58 group xs)
       | otherwise = (f58 group xs)

--59. Solve exercise 31. from Lab 2 using map and filter. 
filter5 :: [(String, Integer)] -> [(String,Integer)]
filter5 l = filter (maimareca5) l where 
    maimareca5 (name, grade)
          | grade > 5 = True
          | otherwise = False
--varianta pattern matching pentru split
{-
	daca c == ' '
		construiesc sirul " "
	altfel
		adaug caracterul la ultimul sir creat	
-}
split1 :: String -> [String]
--daca am ajuns la sfarsit, cream sirul vid(la intoarcerea din recursivitate, se vor adauga caractere la acest sir)
split1 [] = [""]
--se creeaza celule recursiv de la sfarsit spre inceput
--cand se intalneste caracterul spatiu, se creeaza o noua celula cu sirul vid
--altfel se adauga caracterul curent la primul sir din lista(ultimul construit)
split1 (' ':cs) = ("" : split1 cs)
split1 (c:cs) = ((c:cellCompletion) : otherCells)
 where (cellCompletion : otherCells) = split1 cs

names :: [String] -> Bool
names l 
   | (((length l) >= 3) == True) = True
   | otherwise = False

addbonus :: [([String], Integer)] -> [([String], Integer)]
addbonus [] = []
addbonus ((s, medie):l)
   | ((names s) == True) = ((s, medie + 1) : addbonus(l))
   | otherwise = ((s, medie) : addbonus(l))

f59 :: [(String, Integer)] -> [([String], Integer)]
f59 l = addbonus $ map rezolv (filter5 l) where
                      rezolv :: (String, Integer) -> ([String], Integer)
                      rezolv a = (split1 (fst a), (snd a))

--FOLDS

--60. Write a function which appends a list of lists of integers:
app :: [[Integer]] -> [Integer]
app l = foldr (++) [] l

--61. Write the same function tail-recursively.
--Are the two functions identical? Why?
f61 :: [[Integer]] -> [Integer]
f61 l = fp [] l
        where fp x [] = x
              fp x (l:ls) = fp (x++l) ls

--62. Implement foldr (see lecture). 
myfoldr :: (a->b->b) -> b -> [a] -> b
myfoldr op acc [] = acc
myfoldr op acc (x:xs) = op x (myfoldr op acc xs)

--63. Implement foldl (see lecture).
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl op acc [] = acc
myfoldl op acc (x:xs) = myfoldl op (op acc x) xs

--64. Implement list concatenation using a fold. 
concatenation :: [a] -> [a] -> [a]
concatenation l1 l2 = foldr (:) l2 l1
--65. Implement list reversal using a fold. 
reversal :: [a] -> [a]
reversal l = foldl (flip(:)) [] l

--66. Implement the string-splitting function from exercise 52 using folds.
--(Hint: thing about what the accumulator should hold) ??
{-
splitBy :: Char -> String -> [String]
splitBy c = foldr op [[]] 
              where op x (y:ys)
                      | x == c = []:(y:ys)
                      | otherwise = (x:y):ys
-}
f66 :: [Integer] -> (Integer -> Bool)
f66 l = foldr op (\y -> False) l where op x acc = \e-> (e == x) || (acc e)

--67. Implement exercise 53 using a fold. ??

f67 :: [Integer -> Bool] -> Integer -> Bool
f67 l = foldr op (\y -> False) l where op x acc = \e-> (x e) || (acc e)
