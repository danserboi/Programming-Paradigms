import Data.List
--19 Extract the fourth element from a list of integers:
extract :: [Integer] -> Integer
extract list = (head (tail (tail (tail list))))
--20 Write the same function which returns 0 if the list contains less that four elements.
extract2 :: [Integer] -> Integer
extract2 list
             | ((length list) > 3) = (head (tail (tail (tail list))))
             | otherwise = 0
--21 In Haskell, one can use patterns to examine the contents of lists, pairs and other dataypes.
f (x:y:z:w:l) = w
f _ = 0
f2 (x:y:z:w:l) = w
--22 Write a function which filters out all negative numbers
filterNeg :: [Integer] -> [Integer]
filterNeg [] = []
filterNeg (x:list) = if x > 0 then x:(filterNeg list) else (filterNeg list)
--23. What is the type of the function:
f3 x y = (x,y)
--24. What is the type of the function:
f44 'a' _ = []
f44 x y = x:y
--25
--Let f25 "321CB" [("321CB", ["Matei", "Andrei", "Mihai"]), ("322CB",["George, Matei"])]
--  = ["Matei", "Mihai"]

--What is the signature of f?
--What does f do?
--Write an implementation for f:
-- xs :: [(String, [String])]
-- [xs] ::
cuM :: [String] -> [String]
cuM [] = []
cuM ((c:sub):students)
       | c == 'M' = (c:sub) : (cuM students)
       | otherwise = (cuM students)

f25 :: String -> [(String, [String])] -> [String]
f25 group [] = []
f25 group ((gr, students):xs)
       | group == gr = (cuM (students) ++ (f25 group xs))
       | otherwise = (f25 group xs)
--26. Write a function which returns true if the third largest element from a list is positive

f26 :: [Integer] -> Bool
f26 [] = False
f26 y 
   | (head(tail (tail (reverse(sort y)) ) ) > 0) == True = True
   | otherwise = False

--27. Sometimes it really helps to define function via function composition. For instance:

fcompus x = (g.h) x
    where g x = 2*x
          h x = x + 1

fcompus2 x = ff x
    where g x = 2*x
          h x = x + 1
          ff = fcompus2.h
--28. Rewrite exercise 26 via function composition:

f28 :: [Integer] -> Bool
f28 [] = False
f28 y 
   | (((head.tail.tail.reverse.sort) y) > 0) == True = True
   | otherwise = False
{-
29.(hard) Write a function which takes a list of pairs - student-name and grade, removes
    those with grades <5, splits the name in substrings, and adds one point bonus to
    people with three names:

    f [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] =
    	[(["Dan", "Matei", "Popovici"],10),(["Andrei,Alex"],6)]
-}
peste5 :: [(String, Integer)] -> [(String, Integer)]
peste5 [] = []
peste5 ((s, medie):l)
   | (medie > 5) == True = ((s, medie):(peste5 l))
   | otherwise = (peste5 l)
--varianta pattern matching pentru split
{-
	daca c == ' '
		construiesc sirul " "
	altfel
		adaug caracterul la ultimul sir creat	
-}
split :: String -> [String]
--daca am ajuns la sfarsit, cream sirul vid(la intoarcerea din recursivitate, se vor adauga caractere la acest sir)
split [] = [""]
--se creeaza celule recursiv de la sfarsit spre inceput
--cand se intalneste caracterul spatiu, se creeaza o noua celula cu sirul vid
--altfel se completeaza prima celula din lista(ultimul sir construit) cu caracterul curent
split (' ':cs) = ("" : split cs)
split (c:cs) = ((c:cellCompletion) : otherCells)
 where (cellCompletion : otherCells) = split cs

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


--35. What does the function below do?

f35 l = (((++) "?").((:) '>')) l


--How about?

f352 = (((:) '>').(++"?"))

--36. Write a function of a single line, such that:

--f "Matei" = "[Matei]"

ex36 :: String -> [String]
ex36 x = [x]

--37. Use only (:) to solve exercise 36

ex37 :: String -> [String]
ex37 x = x:[]

--38. What does the function ($) to? 
--    (Use :t, and make up some tests)

--39. Write the following implementation using only ($): ???

--f "||" "Matei" = "||Matei||"

--ex39 
ex39 :: String -> String -> String
ex39 s1 s2 = (s1++(s2++s1))

--VARIANTA VECHE LABORATOR 2
{-|
 - Paradigme de Programare CB
 - Laborator 2
 -}

-- 1
subFrom :: Int -> [Int] -> Int
subFrom el [] = el
subFrom el xs = subFrom (el - (head xs)) (tail xs)

-- 2
reverseFold :: (Eq a) => [a] -> [a]
reverseFold list = foldl (flip (++)) [] (map (\x -> [x]) list)

-- 3
prefix123 :: [Int] -> [Int]
prefix123 l = [1, 2, 3] ++ l

-- 4.a
elimFold :: String -> String
elimFold list = foldr f [] list
                where f x | elem x "a" = id
                          | otherwise = (x:)

-- 4.b
elimFilter :: String -> String
elimFilter xs = filter (not . (`elem` "a")) xs