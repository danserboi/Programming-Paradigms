import Data.List

-- 1. Consider the following datatype which encodes λ-expressions:

data LExpr = Var Char | Lambda Char LExpr | App LExpr LExpr | 
             Lambda2 [Char] LExpr | App2 [LExpr]


show_expr :: LExpr -> String
show_expr (Var c) = [c]
show_expr (Lambda c lexpr) = "λ" ++ [c] ++ "." ++ (show_expr lexpr)
show_expr (App lexpr1 lexpr2) = "(" ++ (show_expr lexpr1) ++ " " ++ (show_expr lexpr2) ++ ")"
show_expr (Lambda2 cs lexpr) = "λ" ++ (intersperse ' ' cs) ++ "." ++ (show_expr lexpr)
show_expr (App2 ls) = "(" ++ (show_2 ls) ++ ")"
                       where
                       show_2 [] = ""
                       show_2 (x:[]) = (show_expr x)
                       show_2 (x:xs) = (show_expr x)++" "++(show_2 xs)

--Enroll LExpr in class Show. 
instance Show LExpr where
    show = show_expr

-- 2. Write a function vars which returns a list of variables used in a λ-expression:
rmdups :: [Char] -> [Char]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

vars :: LExpr -> [Char]
vars (Var c) = [c]
vars (Lambda c lexpr) = [c]++(vars lexpr)
vars (App lexpr1 lexpr2) = vars(lexpr1) ++ vars(lexpr2)

vars2 l = rmdups (vars l)
-- 3. Write a function reducible which tests if an expression can be reduced to another. 
-- Write tests first! What are the cases when an expression is reducible?
reducible :: LExpr -> Bool
reducible (App (Lambda c lexpr) _) = True
reducible (App lexpr1 lexpr2) = (reducible lexpr1) || (reducible lexpr2)
reducible _ = False

-- reducible (App (App (Lambda 'x' (Var 'x')) (Var 'x')) (Var 'y'))

-- 4. Write a function which renames all occurrences of a variable with another, 
-- in a λ-expression:

rename :: Char -> Char -> LExpr -> LExpr
rename c1 newc2 (Var c)
       | c1 == c = (Var newc2)
       | otherwise = (Var c)
rename c1 newc2 (Lambda c lexpr)
       | c1 == c = (Lambda newc2 (rename c1 newc2 lexpr))
       | otherwise = (Lambda c (rename c1 newc2 lexpr))
rename c1 newc2 (App lexpr1 lexpr2) = (App (rename c1 newc2 lexpr1) (rename c1 newc2 lexpr2))

-- rename 'x' 'z' (App (App (Lambda 'x' (Var 'x')) (Var 'x')) (Var 'y'))

-- 5. Write a function which replaces all occurrences of a variable with a λ-expression, in a λ-expression:

replace :: Char -> LExpr -> LExpr -> LExpr
replace c1 lexp (Var c)
       | c1 == c = lexp
       | otherwise = (Var c)
replace c1 lexp (Lambda c lexpr) = (Lambda c (replace c1 lexp lexpr))
replace c1 lexp (App lexpr1 lexpr2) = (App (replace c1 lexp lexpr1) (replace c1 lexp lexpr2))

-- replace 'y' (Lambda 'y' (Var 'z')) (App (App (Lambda 'x' (Var 'x')) (Var 'x')) (Var 'y'))

-- 6. Write a function which takes a λ-expression of the form (λx.<body> <arg>) 
--and reduces it in a SINGLE step.

--    What should (λx.(x x) y) produce?
--    What should (λx.λx.(x x) y) produce?

reduction :: LExpr -> LExpr
reduction (App (Lambda c (Lambda c2 l)) l2)
                    | c == c2 = (Lambda c l)
                    | otherwise = replace c l2 (Lambda c2 l)   
reduction (App (Lambda c l) l2) = replace c l2 l

-- reduction (App (Lambda 'x' (App (Var 'x') (Var 'x'))) (Var 'y'))
-- reduction (App (Lambda 'x' (Lambda 'x' (App (Var 'x') (Var 'x')))) (Var 'y'))

--7. Add two data constructors to the type LExpr so that we can also model functions 
--and applications in uncurry form. Examples: (λx y z.<body>), (f x y z). 

--8. Write a proper display function for these new constructors.

--9. Write a function lcurry which takes an uncurries λ-expression 
--and transforms it in curry form.

lcurry :: LExpr -> LExpr 
lcurry (Lambda2 chars lexpr) = foldr op lexpr chars
                  where op char lexpr = (Lambda char lexpr)
lcurry (App2 lexprs) = foldl op (head lexprs) (tail lexprs)
                  where op lexpr1 lexpr2 = (App lexpr1 lexpr2)

--lcurry (Lambda2 ['x','y','z'] (Var 'x'))
--lcurry (App2 [(Var 'f'),(Var 'x'),(Var 'y'),(Var 'z')])

--10. Write a function luncurry which takes a curried λ-expression 
--and transforms it in uncurry form.
--App (App (App (Var 'f') (Var 'x')) (Var 'y')) (Var 'z')
--(((f x) y) z)   becomes    (f x y z)
--(((f ((g a) b)) y) ((h u) v))  becomes  (f (g a b) y (h u v))
luncurry :: LExpr -> LExpr
luncurry (Var c) = (Var c)
luncurry (Lambda c (Lambda c2 lex2)) = luncurry (Lambda2 ([c]++[c2]) lex2)
luncurry (Lambda c lex2) = (Lambda c lex2)
luncurry (Lambda2 str (Lambda c lex2)) = luncurry (Lambda2 (str++[c]) lex2)
-- daca nu intra pe cea anteriora, inseamnca ca nu mai trebuie sa facem uncurry
-- pt prima lambda expresie(deja construita in forma uncurry)
-- dar este posibil sa facem pentru cea de-a doua 
luncurry (Lambda2 str lex2) = Lambda2 str (luncurry lex2)
luncurry (App (App2 l1) l2) = luncurry (App2 (l1++[luncurry l2]))
luncurry (App l1 (App2 l2)) = luncurry (App2 ([luncurry l1]++l2))
luncurry (App l1 l2) = luncurry(App2 ([luncurry l1]++[luncurry l2]))
luncurry (App2 ((App2 l):x)) = luncurry (App2 (l++x))
luncurry (App2 l) = App2 l

-- luncurry $ App (App (App (Var 'f') (App (App (Var 'g') (Var 'a')) (Var 'b'))) (Var 'y')) (App (App (Var 'h') (Var 'u')) (Var 'v'))

-- 11. Write the function fv which computes the list of all free variables of a λ-expression.

fv :: LExpr -> [Char]
fv lexpr = rmdups (free_occs lexpr)

free_occs :: LExpr -> [Char]
free_occs (Var x) = [x]
free_occs (App e e') = (free_occs e)++(free_occs e')
free_occs (Lambda x e) = filter (/= x) (free_occs e)

-- fv $ App (Lambda 'x' (Lambda 'y' (Lambda 'z' (App (Var 'x') (Var 'x'))))) (Lambda 'y' (Lambda 'z' (App (Var 'x') (Var 'x'))))
-- fv (Lambda 'y' (Lambda 'x' (App (Var 'x') (Var 'x'))))

-- 12. Write a function bv which computes the list of all bound variables of a λ-expression. 

bv :: LExpr -> [Char]
bv l = rmdups(elim_free_vars (all_vars l) (fv l))

all_vars :: LExpr -> [Char]
all_vars (Var c) = [c]
all_vars (Lambda c l) = [c]++(all_vars l)
all_vars (App l1 l2) = (all_vars l1)++(all_vars l2)

elim_free_vars :: [Char] -> [Char] -> [Char]
elim_free_vars vars [] = vars
elim_free_vars vars (f:free) = elim_free_vars (filter (/=f) vars) free

-- bv $ App (Lambda 'x' (Lambda 'y' (Lambda 'z' (App (Var 'x') (Var 'x'))))) (Lambda 'y' (Lambda 'z' (App (Var 'x') (Var 'x'))))
-- bv (Lambda 'y' (Lambda 'x' (App (Var 'x') (Var 'x'))))

-- 13. Write a function subst which computes the textual substitution of all free occurrences
-- of some variable x by e in e', according to the lecture definition:

subst :: Char -> LExpr -> LExpr -> LExpr
subst c e (App l1 l2)
          | (elem c (fv (App l1 l2))) = App (subst c e l1) (subst c e l2)
          | otherwise = App l1 l2
subst c e (Lambda c2 l) 
          | (elem c (fv (Lambda c2 l))) = (Lambda c2 (subst c e l))
          | otherwise = (Lambda c2 l)
subst c e (Var c2)
          | c == c2 = e
          | otherwise = (Var c2)

-- subst 'z' (Lambda 't' (Var 't')) (App (Var 'x') (Var 'z'))
-- subst 'x' (Lambda 't' (Var 't')) (Lambda 'y' (Lambda 'x' (App (Var 'x') (Var 'z'))))

--  14. Implement a function which reduces a reducible λ-expression to an irreducible one. (According to the lecture definition, what happens with λx.(λx.x x) ?

reduce :: LExpr -> LExpr
reduce (App (Lambda c l) l2) = ret_exp (replace c (Lambda c l) l2)
reduce (App (App (Lambda c l) l2) l3) = reduce $ (App(ret_exp(replace c (Lambda c l) l2)) l3)
reduce (App (Var c) l) = (App (Var c) (reduce l))
reduce (Lambda c lex2) = (Lambda c (reduce lex2))
reduce l = l

ret_exp (Lambda c l) = l

-- 15. Implement normal-order evaluation. 
norm_eval :: LExpr -> LExpr
-- red
norm_eval (App (Lambda c lex1) lex2) = norm_eval (subst c lex2 lex1)
-- app1
norm_eval (App e e') 
   | reducible e = norm_eval (App (reduce e) e')
   | otherwise = (App e e')
norm_eval lex = lex

-- norm_eval $ App (Lambda 'x' (Var 'z')) (App (Lambda 'y' (App (Var 'y') (Var 'y'))) (Lambda 'x' (App (Var 'x') (Var 'x'))))
-- norm_eval $ App (Lambda 'x' (Lambda 'y' (App (Var 'x') (Var 'x')))) (App (Lambda 'z' (Var 'z')) (Lambda 'z' (Var 'y')))

-- 16. Implement applicative (strict) evaluation. 
app_eval :: LExpr -> LExpr
-- red
app_eval (App (Lambda c lex1) lex2) = app_eval (subst c lex2 lex1)
--if reducible e' app2 else app1
app_eval (App e e') 
  | ((reducible e)||(reducible e')) = (if (reducible e') then App e (reduce e') else App (reduce e) e')
  | otherwise = (App e e') 
app_eval lex = lex

-- app_eval $ App (Lambda 'x' (Lambda 'y' (App (Var 'x') (Var 'x')))) (App (Lambda 'z' (Var 'z')) (Lambda 'z' (Var 'y')))
-- app_eval $ App (Lambda 'x' (Lambda 'z' (App (Var 'x') (Var 'x')))) (Lambda 'z' (Var 'y'))