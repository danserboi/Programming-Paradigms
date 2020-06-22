-- 1. Consider the following recurrence scheme described informally below. 
-- Use it to build the sequence 1,2,…,n!,…
{-
     4  4*5   4*5*6  ...
  3  3  3     3
  ----------------------
  3 3*4 3*4*5 3*4*5*6 ...    
-}

fact = 1 : (zipWith (*) fact [2..])

-- 2. Define the sequence 1,1/2,…,1/k!,…
fact_fraction = map (1/) fact
 
-- 3. Write a function which takes a sequence (an)n≥0 
-- and computes the sequence (sn)n≥0 where sk=∑k≥0ak . 
-- Use a strategy similar to that from exercise 1.
s a = 0 : (zipWith (+) (s a) a)

-- 4. Write the stream of approximations of e.
e_aproximations = map (+1) (s fact_fraction)

-- 5. Write a function which takes a value d , a sequence of approximations (an)n≥0 and returns that value ak from the sequence which satisfies the condition ∣ak−ak+1∣≤d
ex5 d (x:y:xs) = if abs (x - y) <= d then x else (ex5 d (y:xs))

-- 6. Write a function which takes an f , a value a0 and computes the sequence a0,f(a0),f(f(a0)),…
gen :: (a -> a) -> a -> [a]
gen f x = x:(gen f (f x))

-- 7. The sequence (an)n≥0 defined as ak+1=(ak+n/ak)/2 , 
-- will converge to √n as k approaches infinity. 
-- Use it to write a function which approximates √n within 3 decimals. 

sqrt_sequence_list a0 n = a0:(sqrt_sequence_list ((a0+n/a0)/2) n)
approx_sqrt n = ex5 (1/1000) (sqrt_sequence_list 1 n)

{-
8. The diagram below illustrates the approximation of an integral of a continuous function f between two points a and b . The simplest approximation is the area of the rectangle defined by points a and b on the Ox axis, and points f(a) and f(b)

.

To determine a better approximation, the interval

[ [a,b] ] is broken in half and we add up the areas of the rectangles:

    a,m,f(a),f(m) and m,b,f(m),f(b)
Oy
   ^                           
f m|. . . . . . .  --------
   |              / '      \   
   |             /  '       \  f    
f b|. . . . . . /. .'. . . . .-------     
   |       -----    '         '
f a|. . . /         '         '
   |     /          '         '
   |    / '         '         '
   |   /  '         '         '
   ------------------------------>  Ox
          a         m         b

The process can be repeated by recursively dividing up intervals. 
Write the a function integral which computes the sequence of approximations of ∫a-b f(x) . 
-}

integral :: (Float -> Float) -> Float -> Float -> [Float]
integral f a b = (area f a b) : (zipWith (+) integral1 integral2) 
        where m = (a + b) / 2
              integral1 = (integral f a m)
              integral2 = (integral f m b)

area :: (Float -> Float) -> Float -> Float -> Float
area f a b = ((f b) + (f a)) * (b - a) / 2

-- Consider a representation of maps (with obstacles as follows): 

l1="   #     "
l2=" #   # # "
l3=" # ### # "
l4=" #     # "
l5=" ####### "
l6="         "
 
data Map = Map [String]
 
instance Show Map where
  show (Map m) = "\n" ++ foldr (\x acc->x++"\n"++acc) [] m   

m = Map [l1,l2,l3,l4,l5,l6]
 
type State = (Int,Int)

-- 10. Write the function at which returns the value of the position x,y in the map: 
at :: Map -> Int -> Int -> Maybe Char
at (Map map_mat) i1 i2
    | (i1 < 0 || i2 < 0) = Nothing
    | i1 >= (length map_mat) = Nothing
    | i2 >= (length (map_mat !! i1) ) = Nothing
    | otherwise = Just (map_mat !! i1 !! i2)

--11. Define a function which computes, for a given position, the list of valid next positions
-- (a valid position is one that is on the map, and it is not a wall, i.e. a #). 
-- Hint, use the list 
-- [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]. 

next_positions :: Map -> (Int,Int) -> [(Int, Int)]
next_positions (Map map_mat) (x,y) = filter (valid_pos (Map map_mat)) l
    where l = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

valid_pos :: Map -> (Int,Int) -> Bool
valid_pos (Map map_mat) (i1,i2)
    | (i1 < 0 || i2 < 0) = False
    | i1 >= (length map_mat) = False
    | i2 >= (length (map_mat !! i1) ) = False
    | (map_mat !! i1 !! i2) == '#' = False
    | otherwise = True

-- 12. Implement the type Tree a of trees with arbitrary number of children nodes. 
data Tree a = Void | Node [Tree a] a

-- 13. Enrol Tree in class Functor (see classes), and define the function fmap.
instance Functor Tree where
    fmap func Void = Void
    fmap func (Node children value) = Node (map (fmap func) children) (func value)