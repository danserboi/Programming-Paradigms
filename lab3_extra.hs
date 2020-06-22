--1. Definiti operatorul $
dollar :: (a -> b) -> a -> b
dollar f x = f x
g x = x + 1
--2. Care este tipul operatorului de compunere al functiilor? Definiti-l printr-o functie myComp. 
punct :: (b -> c) -> (a -> b) -> a -> c
punct g f x = g (f x)


f x = map (+1) $ g x
    where
        g [] = []
        g (s:xs) = (s:(g xs))
f2 x = map (+1) $ x

--3. Consideram urmatoarele functii lambda care definesc
f3 x = \y -> x + y

f4 = \x -> \y -> x + y

f5 = \x y -> x + y

{-
4. Putem defini un set ca fiind o functie de tipul s :: Integer → Bool, s x returneaza true daca x se afla in setul s.

    Scrieti functia s
    Definiti setul f = {1,2,3} (tot ca o functie)
    Scrieti o functie n pentru multimea numerelor naturale
    Scrieti, in 2 moduri, intersectia a 2 seturi.
    (hard) Scrieti o functie care primeste o lista de seturi si calculeaza intersectia acestora
-}
--daca x apartine set-ului returneaza true, altfel false
s :: Integer -> Bool

s x
   | x == 1 = True
   | x == 2 = True
   | x == 3 = True
   | otherwise = False

nat :: Integer -> Bool
nat x
   | x >= 0 = True
   | otherwise = False

int :: (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
int set set2 x = if ((set x) == True && (set2 x) == True) then True else False
--int set set2 x = ((set x) && (set2 x))
--5. Definiti o functie myFilter care primeste o functie g :: Integer → Bool, o lista de numere intregi,
-- si returneaza o lista cu numerele pentru care g returneaza True. 
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter g [] = []
myFilter g (x:xs)
    | ((g x) == True) = (x:(myFilter g xs))
    | otherwise = (myFilter g xs)
--7. Implementati functia map. Folosind functia map, scrieti o functie care primeste o lista de Bool, si returneaza o lista cu 1 in loc de True si 0 in loc de False. 
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = ((f x):(mymap f xs))

trbool :: Bool -> Integer
trbool True = 1
trbool False = 0

ex7 :: [Bool] -> [Integer]
ex7 x = (mymap trbool x)


--8. Folosind map si/sau filter, scrieti o functie care sa implementeze comportamentul:
--f "321CB" [("321CB", ["Matei", "Andrei", "Mihai"]), ("322CB",["George, Matei"])] = ["Matei", "Mihai"]

numecuM :: String -> Bool
numecuM [] = False
numecuM (c:sub)
      | c == 'M' = True
      | otherwise = False

cuM :: [String] -> [String]
cuM [] = []
cuM students = (myFilter numecuM students) 


ex8 :: String -> [(String, [String])] -> [String]
ex8 group [] = []
ex8 group ((grupa, l):xs)
    | group == grupa = (cuM l) ++ (ex8 group xs)
    | otherwise = [] ++ (ex8 group xs)

-- 9. Folosind map si/sau filter, scrieti o functie care sa implementeze comportamentul:
--f [("Dan Matei Popovici",9),("Mihai",4),("Andrei Alex",6)] = [(["Dan", "Matei", "Popovici"],10),(["Andrei,Alex"],6)]
--e aproape facuta in lab2v2.hs

--Ex 10
myConcat [] = []
myConcat (x:xs) = x ++ myConcat(xs)