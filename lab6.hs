-- 1. A dictionary is a collection of key-value pairs, 
--which we can represent in Haskell as a list of pairs (String,Integer) 
--where String is the key type and Integer is the value type:

type Dict = [(String,Integer)]

--Implement the function valueOf which takes a key and a dictionary,
--and returns the associated value. It is guaranteed that the value exists.

valueOf :: String -> Dict -> Integer
valueOf key (d:ds) 
    | (key == (fst d)) = snd d
    | otherwise = valueOf key ds

-- 2. Implement the function ins which takes a key s, a value i, a dictionary d 
--and updates the value of s in the dictionary, if the value exists, 
--or adds the key-value pair, otherwise. 
--For instance ins “x” 1 [(“x”,0)] = [(“x”,1)] 
--and ins “x” 1 [(“y”,0)] = [(“x”,1),(“y”,0)].

ins :: String -> Integer -> Dict -> Dict
ins key number d = ins_acc key number d []

ins_acc key number [] acc = [(key, number)] ++ acc
ins_acc key number (d:ds) acc
    | (key == (fst d)) =  acc ++ [(key, number)] ++ ds
    | otherwise = (ins_acc key number ds (acc++[d]))

-- 3. Consider the type PExpr of program expressions defined in the lecture. 
-- Implement a function for showing PExpr values:

data PExpr = Val Integer |
             Var String  |
             PExpr :+: PExpr
 
show_pexpr :: PExpr -> String
show_pexpr (Val x) = show x
show_pexpr (Var s) = s
show_pexpr (p1 :+: p2) = (show_pexpr p1)++" + "++(show_pexpr p2)     

--4. Implement a function which takes a dictionary of program variables, 
--a program expression PExpr, and evaluates it. 
--For instance: eval_pexpr [(“x”,2),(“y”,1)] ( (Var "x") :+: (Var "y") ) returns 3. 
eval_pexpr :: Dict -> PExpr -> Integer
eval_pexpr dict (Val x) = x
eval_pexpr dict (Var s) = (valueOf s dict)
eval_pexpr dict (p1 :+: p2) = (eval_pexpr dict p1)+(eval_pexpr dict p2)

-- 5. Consider the type BExpr of boolean expressions defined in the lecture.
--Implement a function for displaying values of BExpr:

data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not BExpr |
            BExpr :&&: BExpr 
 
show_bexpr :: BExpr -> String
show_bexpr (p1 :==: p2) = (show_pexpr p1)++" == "++(show_pexpr p2)
show_bexpr (p1 :<: p2) = (show_pexpr p1)++" < "++(show_pexpr p2)
show_bexpr (Not b) = "~("++(show_bexpr b)++")"
show_bexpr (b1 :&&: b2) = "("++(show_bexpr b1)++") && ("++(show_bexpr b2)++"):"

{-
show_bexpr (((Var "x") :+: (Var "y")) :==: (Var "z"))
show_bexpr ((((Var "x") :+: (Var "y")) :<: (Var "z")):&&:(((Var "x") :+: (Var "z
")) :<: (Var "y")))
-}

--Add the following code to your program:

instance Show PExpr where
    show = show_pexpr
 
instance Show BExpr where
    show = show_bexpr    

-- 6. Write a function which, given a dictionary, evaluates boolean conditions BExpr:

eval_bexpr :: Dict -> BExpr -> Bool
eval_bexpr dict (p1 :==: p2) = (eval_pexpr dict p1) == (eval_pexpr dict p2)
eval_bexpr dict (p1 :<: p2) = (eval_pexpr dict p1) < (eval_pexpr dict p2)
eval_bexpr dict (Not b) = (not (eval_bexpr dict b))
eval_bexpr dict (b1 :&&: b2) = (eval_bexpr dict b1) && (eval_bexpr dict b2)

-- Add the following code (from the lecture) to your program:

type Var = String

data Prog = PlusPlus Var |        -- x++;
            Var :=: PExpr |     -- x = <expr>;
            DeclareInt Var |      -- int x;
            Begin Prog Prog |     -- <p> <p'>
            While BExpr Prog |     -- while (<expr>) { <p> }
            If BExpr Prog Prog      -- if (<expr>) { <p> } else { <p'> }   
 
show_p :: Prog -> String
show_p (PlusPlus v) = v++"++;\n"
show_p (x :=: e) = x++" = "++(show_pexpr e)++";\n"
show_p (DeclareInt x) = "int "++x++";\n"
show_p (Begin p p') = (show_p p)++(show_p p')
show_p (While e p) = "while ("++(show_bexpr e)++") {\n"++(show_p p)++"}\n"
show_p (If e p p') = "if ("++(show_bexpr e)++") {\n"++(show_p p)++"}\n else {\n"++(show_p p')++"}\n"
 
instance Show Prog where
  show = show_p

-- 7. Define the program:
{-

int x;
x++;
while (x < 100){
   x = x + 1
}

-}
p7 = Begin (DeclareInt "x") $ 
     Begin (PlusPlus "x") $ 
     While ((Var "x") :<: (Val 100)) 
                    ("x" :=: ((Var "x") :+: (Val 1))
                   )
-- 8. Define a function eval which takes a dictionary and a program,
--and evaluates the program under the given dictionary.
--The function will return an updated dictionary. For instance:
{-
eval [("x",0)] (PlusPlus "x") = [("x",1)]
eval [("x",1)] ("x" :=: ((Var "x") :+: (Val 1))) = [("x",2)]
eval [] (DeclareInt "x") = [("x",0)]
eval [] p7
eval [] ex
-}
ex = (Begin (DeclareInt "x") (PlusPlus "x"))
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

-- Add the following error-handling type to your program:

data Result a = Error String |  -- a value of type (Result a) is an error, or
                Value a deriving Show        -- an actual value, wrapped by the data constructor Value

--9. Implement the function check which takes a list of defined variables,
--a program p, and returns an updated list of variable definitions, 
--if p does not contain undeclared variables, and an error message, otherwise.

declared_vars :: Prog -> [Var]
declared_vars (DeclareInt v) = [v]
declared_vars (Begin p p') = (declared_vars p)++(declared_vars p')
declared_vars (While _ p) = declared_vars p
declared_vars (If _ p p') = (declared_vars p)++(declared_vars p')
{-
For instance:

check [] (DeclareInt "x") = Value ["x"]
check [] (PlusPlus "x") = Error "x is undefined"
check ["x"] (PlusPlus "x") = ["x"]
-}
--varsList (Error string) = []
varsList :: Result [Var] -> [Var]
varsList (Value []) = []
varsList (Value (x:xs)) = (x : (varsList (Value xs)))

isError :: Result a -> Bool
isError (Error _) = True
isError _ = False

wrong = (Begin (DeclareInt "x") (PlusPlus "y"))

check :: [Var] -> Prog -> Result [Var]
check vars (DeclareInt v) = Value (v:vars)
check vars (PlusPlus v)
        | (elem v vars) = (Value vars)
        | otherwise = (Error (v++" is undefined"))
check vars (v :=: e)
        | (elem v vars) = (Value vars)
        | otherwise = (Error (v++" is undefined"))
check vars (Begin p p')
        | (isError (check vars p)) = (check vars p)
        | otherwise = (check (varsList (check vars p)) p')
check vars (While e p) = check vars p
check vars (If e p p')
        | (isError (check vars p)) = (check vars p)
        | otherwise = (check (varsList (check vars p)) p')