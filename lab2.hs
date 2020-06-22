import Data.List
--20. Implement insertion sort.
myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (y:ys) = if x < y 
                 then x:y:ys 
         else y : myInsert x ys
inssort :: [Int] -> [Int]
inssort [] = []
inssort (x:xs) = myInsert x (inssort xs)

--21. Extract the fourth element from a list of integers
extract :: [Integer] -> Integer
extract list = (head (tail (tail (tail list))))

--22. Write the same function which returns 0 if the list contains less that four elements and 0 otherwise. 
extract2 :: [Integer] -> Integer
extract2 list
             | ((length list) > 3) = (head (tail (tail (tail list))))
             | otherwise = 0

--PATTERN MATCHING

--23. What are the types of x,y,z,w and that of l in the implementation below?

f231 (x:y:z:w:l) = w
f231 _ = 0

--How about:

f232 (x:y:z:w:l) = w

--What is the difference? 
--Pt f231, faptul ca pentru orice lista care nu are cel putin 4 elemente, rezultatul e 0 (numar)
--implica ca rezultatul este intotdeauna un numar.
--La f232 rezultatul poate avea orice tip.

--24. Write a function which filters out all negative numbers from a list. Use patterns.
filterNeg :: [Integer] -> [Integer]
filterNeg [] = []
filterNeg (x:list) = if x > 0 then x:(filterNeg list) else (filterNeg list)

-- 25. What is the type of the function:

f25 x y = (x,y)

--In the body of a function definition, , is a base constructor for pairs. The following are pairs: (1,2), (“Matei”,20), ([1,2],3). Note that there is no restriction on the type of the first and second values of a pair. 

-- 26. What is the type of the function:

f26 'a' _ = []
f26 x y = x:y

--In Haskell, the type String is equivalent to [Char] (hence strings are lists of chars). Strings can be introspected using patterns just like any other list. 

-- 27. Let f “321CB” [(“321CB”, [“Matei”, “Andrei”, “Mihai”]), (“322CB”,[“George", "Matei”])] = [“Matei”, “Mihai”]

--What is the signature of f?

--What does f do (note that Matei and Mihai both start with letter 'M')?

--Write an implementation for f. 
cuM :: [String] -> [String]
cuM [] = []
cuM ((c:sub):students)
       | c == 'M' = (c:sub) : (cuM students)
       | otherwise = (cuM students)

f27 :: String -> [(String, [String])] -> [String]
f27 group [] = []
f27 group ((gr, students):xs)
       | group == gr = (cuM (students) ++ (f27 group xs))
       | otherwise = (f27 group xs)
--28. Write a function which returns True if the third largest element from a list is positive 
f28 :: [Integer] -> Bool
f28 [] = False
f28 y 
   | (head(tail (tail (reverse(sort y)) ) ) > 0) == True = True
   | otherwise = False

--FUNCTION COMPOSITION AND FUNCTION APPLICATION

-- 29. Sometimes it really helps to define function via function composition. For instance:

f29a x = (g.h) x
    where g x = 2*x
          h x = x + 1

-- What type does f have? What type does g have? 
--f29a :: Num c => c -> c

-- What does the following function do?

f29b x = ff x
    where g x = 2*x
          h x = x + 1
          ff = f29b.h
--functia f29b e egala cu functia ff care apeleaza f29b compus cu h=>
--functia se autoapeleaza la nesfarsit, neexistand conditie de oprire

-- 30. Rewrite exercise 28 via function composition:

f30 :: [Integer] -> Bool
f30 [] = False
f30 y 
   | (((head.tail.tail.reverse.sort) y) > 0) == True = True
   | otherwise = False

--  31. Write a function which takes a list of pairs - student-name and grade, removes those with grades less than 5, splits the name in substrings, and adds one bonus point to people with three names. Example:

--    f [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] =
--    	[(["Dan", "Matei", "Popovici"],10),(["Andrei,Alex"],6)]
peste5 :: [(String, Integer)] -> [(String, Integer)]
peste5 [] = []
peste5 ((s, medie):l)
   | (medie > 5) == True = ((s, medie):(peste5 l))
   | otherwise = (peste5 l)

--sau folosind filter:
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

--varianta cu head si tail
split2 :: String -> [String]
split2 [] = [""]
split2 (c:cs)| c == ' '  = ("" : rest)
             | otherwise = ((c : head rest) : tail rest)
    where rest = split2 cs

names :: [String] -> Bool
names l 
   | (((length l) >= 3) == True) = True
   | otherwise = False

addbonus :: [([String], Integer)] -> [([String], Integer)]
addbonus [] = []
addbonus ((s, medie):l)
   | ((names s) == True) = ((s, medie + 1) : addbonus(l))
   | otherwise = ((s, medie) : addbonus(l))

ex31 :: [(String, Integer)] -> [([String], Integer)]
ex31 l = addbonus $ map rezolv (filter5 l) where
                      rezolv :: (String, Integer) -> ([String], Integer)
                      rezolv a = (split1 (fst a), (snd a))

-- 32. Write the signature and implement the following function:

--   f ["Matei", "Mihai"] ["Popovici","Dumitru"] = [("Matei","Popovici"), ("Mihai","Dumitru")]

f32 :: [String] -> [String] -> [(String, String)]
f32 [] [] = []
f32 (x:xs) (y:ys) = (x, y) : f32 xs ys

-- 33. Implement the following function:

--   f ["Matei", "Mihai"] ["Popovici","Dumitru"] = ["MPopovici", "MDumitru"]

f33 :: [String] -> [String] -> [String]
f33 [] [] = []
f33 ((x:xname):xs) (y:ys) = (x:y) : f33 xs ys

-- 34. Sometimes it helps to use functions in infix form. For instance, instead of mod x 2, we can write x mod 2. We can also define infix functions:

--	x "?????" y = x + y ??? E SCRIS GRESIT!

--Implement a function for transforming lists into pairs, in exercise 32. and use it in the infix form. 
f34 :: [a] -> [a] -> [(a, a)]
f34 [] [] = []
f34 (x:xs) (y:ys) = (x, y) : f34 xs ys
-- 35. Just in the same way, we can treat infix functions as prefix. We do this using round parentheses. Test:
--		:t (+)
--		:t (&&)
--		:t (++)
--36. What about function composi-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- 37. What does the function below do? Take parts of the function below, and test them.

f37a l = (((++) "?").((:) '>')) l

-- How about?

f37b = (((:) '>').(++"?"))

-- 38. Write a function of a single lin

f38 :: String -> String
f38 s = (++) ((++) "{" s) "}"

--39. Use only (:), reverse and functional composition . to solve exercise 38 
-- f39 s = ('{':s)
f39 :: String -> String
f39 = ((:) '{').reverse.((:) '}').reverse

-- 40. What does the function ($) to?

--  (Use :t, and make up some tests)
-- 41. Write the following implementation using only ($):

-- f "||" "Matei" = "||Matei||"

f41 :: String -> String -> String
f41 s1 s2 = (s1++) $ reverse $ (s1++) $ reverse s2

--42. Solve exercise 13. from Lab 2 using $ and other improvements (treat the case when the list has fewer elements): 

f42 :: [Integer] -> Bool
f42 l
    | (((length l) < 3) == True) = False
    | ((mod (head $ tail $ tail $ reverse l) 2) == 1) = True
    | otherwise = False
