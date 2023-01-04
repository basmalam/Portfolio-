{-|
Module: A2
Description: Assignment 2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}
-- This lists what this module exports. Don't change this!
module A2
  (
    runStag,
    eval)
where

-- You *may not* add imports from Data.Map, or any other imports
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)


-- | Runs a StagShell expression by calling `eval` with the empty environment
runStag :: Expr -> Value
runStag e = eval Data.Map.empty e


-- | An interpreter for the StagShell language.
eval :: Env -> Expr -> Value
eval env (Literal v) = v
eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x + y) -- todo
    (Error x, y) -> Error x
    (x, Error y) -> Error y
    (x, y) -> Error "Plus" -- todo
    
    -- what other patterns are missing above?
eval env (Times a b) = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x * y) -- todo
    (Error x, y) -> Error x
    (x, Error y) -> Error y
    (x, y) -> Error "Times" -- todo

-- todo: handle Equal, Cons, First, Rest, and If
eval env (Equal a b) = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> if x == y then T else F -- todo
    --(Literal x, Literal y)  -> if x == y then T else F -- todo
    (Error x,  y) -> Error x
    (x , Error y) -> Error y
    (T , T) -> T
    (F , F) -> T
    (F , T) -> F
    (T , F) -> F
    (Pair x y, Pair s t) -> if x == s then if y == t then T else F else F
    (Closure x y z, Closure s t w) -> if x == s then if y == t then if z == w then T else F else F else F
    (x, y) -> if x == y then T else F

eval env (Cons a b) = case ((eval env a), (eval env b)) of
  (Error x,  y) -> Error x
  ( x , Error y) -> Error y
  (x, y) -> Pair x y 

eval env (First a) = case (eval env a) of
  (Pair (Error x) y) -> Error x 
  (Pair x (Error y)) -> Error y
  (Pair x y) -> x
  (x) -> Error "First"

eval env (Rest a) = case (eval env a) of
  (Pair (Error x) y) -> Error x 
  (Pair x (Error y)) -> Error y
  (Pair x y) -> y
  (x) -> Error "Rest"

eval env (If a b c) = case ((eval env a), (eval env b), (eval env c)) of
  (T, Error x, y) -> Error x
  (F, x, Error y) -> Error y
  (T, x, y) -> x
  (F, x, y) -> y
  (Error a, x, y) -> Error a

eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a -- "a" is of type Value 
    Nothing -> Error "Var" -- "name" is not found in "env"

-- todo: handle Lambda and App
eval env (Lambda a b) = case (a, b) of -- todo
  (x,y) -> Closure x env y

eval env (App fnExpr argExprs) = case ((eval env fnExpr), argExprs) of
  (Closure param env body, args) -> if length param == length args && helper args env then eval (helper2 param args env) body else Error "App"
  (x,y) -> Error "App"))

--evaluate function with args 
helper2 :: [String] -> [Expr] -> Env -> Env
helper2 [] [] env = env
helper2 [key] [value] env = Data.Map.insert key (eval env value) env
helper2 (x:xs) (y:ys) env = helper2 xs ys (Data.Map.insert x (eval env y) env) 


-- check if there are any errors
helper :: [Expr] -> Env -> Bool
helper [] e = True
helper [x] e = if (eval e x) == Error "Plus" || (eval e x) == Error "Var" || (eval e x) == Error "Times" 
  || (eval e x) == Error "First" || (eval e x) == Error "Rest" || (eval e x) == Error "App" then False else True
helper (x:xs) e = (helper [x] e) && (helper xs e)

  -- body contains error 
  -- args expr lst contains error 
  -- length of param doesnt match 
  -- type check x is not a list of strings