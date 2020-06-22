-- 1. Define the type list with elements of type Integer:

data IntList = Void | 
               Cell Integer IntList
               deriving Show
--2. Define a function which computes the sum of elements of such a list: 
isum :: IntList -> Integer
isum Void = 0
isum (Cell i l) = i + (isum l)
--3. Define type polymorfic type List a encoding lists with elements of type a: 
data List a = Empty |
              Cons a (List a)
              
              deriving Show
-- 4. Define a function which converts IntList lists to List Integer lists:

to_poly_list :: IntList -> List Integer
to_poly_list Void = Empty
to_poly_list (Cell i l) = (Cons i (to_poly_list l))

--5. Define a function which displays lists. What type will this function have? 

show_list Empty = ""
show_list (Cons e l) = (show e)++" "++(show_list l)

-- Add the tree datatype definition from the lecture:

data Tree a = Voidt | Node (Tree a) a (Tree a) deriving Show

-- 6. Implement the function flatten:

flatten :: Tree a -> List a
flatten Voidt = Empty
flatten (Node l k r) = (flatten l) `app` (Cons k (flatten r))

-- 7. Define list concatenation over type List a:

app :: (List a) -> (List a) -> (List a)
app Empty l = l
app (Cons x xs) l = Cons x (app xs l)

-- 8. Define the function tmap which is the Tree a correspondent 
-- to map::(a→b) → [a] → [b]. 

tmap :: (a -> b) -> (Tree a) -> (Tree b)
tmap f Voidt = Voidt
tmap f (Node l v r) = Node (tmap f l) (f v) (tmap f r)

-- 9. Define the function tzipWith:

tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith f Voidt Voidt = Voidt
tzipWith f (Node l v r) (Node l' v' r') = Node (tzipWith f l l') (f v v') (tzipWith f r r')

--  10. Define the function tfoldr:

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr op acc Voidt = acc
tfoldr op acc (Node l v r) = tfoldr op (op v (tfoldr op acc r)) l

--  11. Implement the flattening function using tfoldr:

tflatten :: Tree a -> List a
tflatten Voidt = Empty
tflatten (Node l k r) = app (tfoldr Cons Empty l) (Cons k (tfoldr Cons Empty r))

--  12. Consider the following definition of natural numbers extended with the value Infinity:

data Extended = Infinity | Value Integer deriving Show

--Define a function which computes the sum of two Extended values:

extSum :: Extended -> Extended -> Extended
extSum _ Infinity = Infinity
extSum Infinity _ = Infinity
extSum (Value v) (Value v') = (Value (v+v'))

-- 13. Define a function which computes the equality of two Extended values:

equal :: Extended -> Extended -> Bool
equal Infinity Infinity = True
equal (Value v) (Value v') = v == v'
equal _ _ = False



--The polymorphic datatype:
--data Maybe a = Nothing | Just a
--which is part of the Haskell default library is useful for error handling. 
--For instance, if a computation fails, a function may return Nothing. 
--If the computation returns a result x of type a, 
--then the function will return a value Just x. 
-- 14. Implement the function lhead which behaves like head but returns 
--Nothing if the argument of the function is the empty list:

lhead :: List a -> Maybe a
lhead Empty = Nothing
lhead (Cons v l) = Just v

--15. Implement the function ltail
ltail :: List a -> Maybe (List a)
ltail Empty = Nothing
ltail (Cons v l) = Just l