{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- 1. Enroll the type Extended shown below, in class Eq:

data Extended = Infinity | Value Integer 

instance Eq Extended where
    Infinity == Infinity = True
    (Value i) == (Value i') = (i == i')
    _ == _ = False

-- 2. Enroll the type Formula a in class Eq: 
{-
data Formula a = Atom a |
                 Or (Formula a) (Formula a) |
                 And (Formula a) (Formula a) |
                 Not (Formula a)

instance Eq a => Eq (Formula a) where
    (Atom atom) == (Atom atom') = atom == atom'
    (Or form1 form2) == (Or form1' form2') = (form1 == form1') && (form2 == form2')
    (And form1 form2) == (And form1' form2') = (form1 == form1') && (form2 == form2')
    (Not form) == (Not form') = form == form'
    _ == _ = False
-}
-- 3. Enroll the type Set a, defined below, in class Num. 
-- Implement operation (+) as set reunion and (*) as set intersection. 
-- Implement also fromInteger which takes an integer x and returns the set {x}. 

data Set a = F (a->Bool)

instance Num (Set a) where
    (F f) + (F f2) = F f3 where
        f3 x = (f x) || (f2 x)
    (F f) * (F f2) = F f3 where
        f3 x = (f x) && (f2 x)
    fromInteger x = F (\x -> True)


-- 4. In the last lab, we have worked with the following datatypes and functions:

type Dict = [(String,Integer)]

data PExpr = Val Integer |
             Var String  |
             PExpr :+: PExpr 
 
valueOf :: String -> Dict -> Integer
valueOf key (d:ds) 
    | (key == (fst d)) = snd d
    | otherwise = valueOf key ds

ins :: String -> Integer -> Dict -> Dict
ins key number d = ins_acc key number d []

ins_acc key number [] acc = [(key, number)] ++ acc
ins_acc key number (d:ds) acc
    | (key == (fst d)) =  acc ++ [(key, number)] ++ ds
    | otherwise = (ins_acc key number ds (acc++[d]))

eval_pexpr :: Dict -> PExpr -> Integer
eval_pexpr dict (Val x) = x
eval_pexpr dict (Var s) = (valueOf s dict)
eval_pexpr dict (p1 :+: p2) = (eval_pexpr dict p1)+(eval_pexpr dict p2)

data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not BExpr |
            BExpr :&&: BExpr 
 
eval_bexpr :: Dict -> BExpr -> Bool
eval_bexpr dict (p1 :==: p2) = (eval_pexpr dict p1) == (eval_pexpr dict p2)
eval_bexpr dict (p1 :<: p2) = (eval_pexpr dict p1) < (eval_pexpr dict p2)
eval_bexpr dict (Not b) = (not (eval_bexpr dict b))
eval_bexpr dict (b1 :&&: b2) = (eval_bexpr dict b1) && (eval_bexpr dict b2)

type Var = String

data Prog = PlusPlus Var |       
            Var :=: PExpr |     
            DeclareInt Var |     
            Begin Prog Prog |     
            While BExpr Prog |     
            If BExpr Prog Prog      
 
eval :: Dict -> Prog -> Dict
eval dict (PlusPlus v) = ins v ((valueOf v dict) + 1) dict
eval dict (x :=: e) = ins x (eval_pexpr dict e) dict
eval dict (DeclareInt x) = ins x 0 dict
eval dict (Begin p p') = (eval (eval dict p) p')
eval dict (While e p)
        | ((eval_bexpr dict e) == True) = (eval (eval dict p) (While e p))
        | otherwise = dict
eval dict (If e p p')
        | ((eval_bexpr dict e) == True) = (eval dict p)
        | otherwise = (eval dict p')                       

{-
We have three different datatypes and three evaluation methods, one for each type.

    The first type models arithmetic expressions and it's evaluation produces an integer
    The second type models boolean expressions (comparisons) and evaluates to a boolean
    The third type models programs and evaluates to a dictionary, holding the final values for each variable

Design a class called Eval which contains a unique eval method. How should the class parameters be?

Enrol types PExpr, BExpr and Prog in this class, thus eliminating the need for multiple evaluation function names.
-}
class Eval a b where
    ev :: Dict -> a -> b

instance Eval PExpr Integer where
    ev d p = eval_pexpr d p
instance Eval BExpr Bool where
    ev d b = eval_bexpr d b
instance Eval Prog Dict where
    ev d p = eval d p