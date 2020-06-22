{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances #-}

{-
    We represent a matrix of integers as a list of rows, each
    row being a list of integers.
    The keyword "type" is useful to create type aliases (similar to typedef in C)
    and make type signatures more legible:
-}

type Matrix = [[Integer]]

{-
    Ex 1. Write a function which takes a string and parses it to a matrix.
          Rows are separated by '\n' and columns, by ' '.
    Example:
    parsem "1 2\n3 4\n" = [[1,2], [3,4]]
-}
parsem :: String -> Matrix
parsem = (map ((map rd).(splitBy ' '))) . (splitBy '\n')
        where   splitBy :: Char -> String -> [String]
                splitBy c = foldr op [] 
                            where
                              op x [] 
                                 | x /= c = [[x]]
                                 | otherwise = [[]]
                              op x (y:ys)
                                 | x /= c = (x:y):ys
                                 | otherwise = []:(y:ys)
                rd x = read x :: Integer

parsem2 :: Char -> Char -> String -> [[String]]
parsem2 colsep linesep = (map (splitBy colsep)) . (splitBy linesep)
        where   splitBy :: Char -> String -> [String]
                splitBy c = foldr op [] 
                            where
                              op x [] 
                                 | x /= c = [[x]]
                                 | otherwise = [[]]
                              op x (y:ys)
                                 | x /= c = (x:y):ys
                                 | otherwise = []:(y:ys)
{-
    Ex 2. Write a function that converts a matrix to a string encoded
          as illustrated in the previous exercise.
          Hint: Start by converting a line to string
          Hint: Test and use the function show
-}

toString :: Matrix -> String
toString m = foldr faux2 "" m

faux2 :: [Integer] -> String -> String
faux2 l1 string2 = ((f l1)++"\n"++string2)


f :: [Integer] -> String
f l = foldr faux "" l

faux :: Integer -> String -> String
faux num string = (show num) ++" "++ string


{-
    Ex 3. Add to your code, the function below.
    Test it.
-}

displaymat = putStrLn . toString

{-
    Ex 4. Write a function that computes the scalar product with
          an integer
-}

vprod :: Integer -> Matrix -> Matrix
vprod int m = map(map(int*)) m

m1 = [[1,2],[3,4]]
m2 = [[5,6],[7,8]]

{-  Ex 5. Write a function which adjoins two matrices by extending rows.
    Example:  1 2   `hjoin` 5 6   =   1 2 5 6
              3 4           7 8       3 4 7 8
-}

hjoin :: Matrix -> Matrix -> Matrix
hjoin [] [] = []
hjoin m1 m2 = (((head m1) ++ (head m2)) : (hjoin (tail m1) (tail m2)))

{-  Ex 6. Write a function which adjoins two matrices by adding new rows 
    Example:  1 2   `vjoin` 5 6   =   1 2 
              3 4           7 8       3 4 
                                      5 6
                                      7 8 
-}

vjoin :: Matrix -> Matrix -> Matrix
vjoin [] [] = []
vjoin m1 m2 = m1++m2
{-  Ex 7. Write a function which adds two matrices -}

msum :: Matrix -> Matrix -> Matrix
msum m1 m2 = zipWith (zipWith (+)) m1 m2

{- Ex 8. Write a function which computes the transposition of a matrix 
   Example:
             1 2    tr    1 3
             3 4   ---->  2 4
-}
m = [[1,2,3],[4,5,6],[7,8,9]]

tr :: [[a]] -> [[a]]
tr ([]:_) = []
tr m = (map head m) : tr(map tail m)

{- 
    Ex 9. Write a function which computes the vectorial product of two
          matrices.
          Hint: start by writing a function which computes
          aij for a given line i and column j (both represented as lists)
          Hint: extend the function so that it computes line 1 of the
                product matrix
-}

mprod :: Matrix -> Matrix -> Matrix
mprod m1 m2 = map(\li->(map(\cj->foldr (+) 0 (zipWith (*) li cj)) (tr m2))) m1

{-
   We can represent images as matrices of pixels. In our example a pixel
   will be represented as a char.
-}
type Image = [[Char]]

l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
l10="      ** *******          ** *******    "
l11="      ** ******           ** ******     "
l12="      ** **               ** **         "
l13="      ** **               ** **         "
l14="      ** **               ** **         "
l15=" **   ** **          **   ** **         "
l16="***   *  *          ***   *  *          "
l17=" ***    *            ***    *           "
l18="  ******              ******            "
l19="    ***                 ***             "

logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]

{- implement an image-displaying function: -}



toStringImg :: Image -> String
toStringImg m = foldr faux2img "" m

faux2img :: [Char] -> String -> String
faux2img l1 string2 = ((fimg l1)++"\n"++string2)


fimg :: [Char] -> String
fimg l = foldr fauximg "" l

fauximg :: Char -> String -> String
fauximg num string = (show num) ++ string




displaym = putStrLn.toStringImg

{-
   Implement a function which flips an image horizontally:
-}

flipH :: Image -> Image
flipH = map reverse

{-
   Implement a function which flips an image vertically:
-}
flipV :: Image -> Image 
flipV = reverse
{-
   Implement a function which rotates an image 90grd clockwise
-}
rotate90r :: Image -> Image
rotate90r img = flipH (tr img)

{-
   Implement a function which rotates an image -90grd clockwise
-}
rotate90l :: Image -> Image
rotate90l img = reverse (tr img)
-- rotate90l img = flipV.tr img
{-
   Implement a function which returns a diamond of a specified height.
   Example:
                          *
                         ***
   diamond 3 =          *****
                         ***
                          *
                         *
   diamond 2 =          ***
                         *
   
   First write a function which produces the first half, then build the lower part
-}
charString :: Integer -> Char -> String
charString 0 c = ""
charString x c = [c]++(charString (x-1) c)

diamondLine :: Integer -> Integer -> String
diamondLine noStars noSpaces = ((charString noSpaces ' ')++(charString noStars '*'))++(charString noSpaces ' ')++"\n"


topOfDiamond :: Integer -> Integer -> Image
topOfDiamond x i 
            | (i < x) = [(diamondLine (2*i-1) (x-i))]++(topOfDiamond x (i+1))
            | otherwise = []
bottomOfDiamond :: Integer -> Integer -> Image
bottomOfDiamond x i = tail $ flipV (topOfDiamond x i)
{-
s
for i = 1; i <= x; i++
s = s ++ [diamondLine 2*i-1 (x-i)]
-}

--deci am 3*2-1 caractere pe linie
--pe prima linie am 1*2-1 stelute si (3-1)*2 spatii
--pe a doua linie am 2*2-1 steluite si (3-2)*2 spatii
--pe a treia linie am 3*2-1 steluite si (3-3)*2 spatii
diamond :: Integer -> Image 
diamond x = (topOfDiamond (x+1) 1)++(bottomOfDiamond (x+1) 1)

{-
   Implement a function with takes two images of the same dimensions. 
   The second image is a mask. 
   The output will be another image in which all pixels from the first image
   overlaying with a '*'-pixel from the mask will be displayed. All others will be
   deleted (made equal to ' ').
   
   Example:
   
   ******                ****     ****
   **  **   `overlay`   ******   **  ** 
   **  **                ****     *  *
   ******                 **       **
-}
lineIntersection :: String -> String -> String
lineIntersection [] [] = []
lineIntersection (x:xs) (y:ys)
                | (x==y) = (x:(lineIntersection xs ys))
                | otherwise = (' ':(lineIntersection xs ys))

overlay :: Image -> Image -> Image
overlay [] [] = []
overlay (im1:im1s) (im2:im2s) = ((lineIntersection im1 im2):(overlay im1s im2s))

--17. Implement a function which places zeros above the diagonal 
--of a square matrix. Use folds. 
ex17 :: Matrix -> Matrix
ex17 m = foldr op [] m where
  op l1 acc = ((modifyLine l1 ((length l1) - ((length acc)))) : acc) 

modifyLine :: [Integer] -> Int -> [Integer]
modifyLine l1 0 = map (\x->0) l1
modifyLine (x:xs) nr = x:(modifyLine xs (nr-1))

--18. Implement a function 
--which places zeros below the diagonal of a square matrix. Use folds. 
ex18 :: Matrix -> Matrix
ex18 m = foldr op [] m where
  op l1 acc = ((modifyLine2 l1 ((length l1) - (length acc) - 1)) : acc) 

modifyLine2 :: [Integer] -> Int -> [Integer]
modifyLine2 l1 0 = map (\x->x) l1
modifyLine2 (x:xs) nr = 0:(modifyLine2 xs (nr-1))

--19. Implement either of 17 or 18 using the function(s) take and drop. 
--Check the types of these and test them. 
ex19 :: Matrix -> Matrix
ex19 m = foldr op [] m where
  op l1 acc = ((modifyLine l1 ((length l1) - ((length acc)))) : acc) 

modifyLine3 :: [Integer] -> Int -> [Integer]
modifyLine3 l1 nr = (take nr l1)++(map (\x->0) (drop nr l1))

--20. Implement a function which computes the diagonal matrix of a square matrix. 
mainDiagonal :: Matrix -> [Integer]
mainDiagonal [] = []
mainDiagonal (x:xs) = head x : mainDiagonal (map tail xs)
--sau
diag :: Matrix -> [Integer]
diag x = zipWith (!!) x [0..((length x)-1)]
