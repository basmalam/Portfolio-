{-|
 -
Module:      A3
Description: Assignment 3
Copyright: (c) University of Toronto, 2021
               CSC324 Principles of Programming Languages, Fall 2021
-}
-- This lists what this module exports. Don't change this!

module A3 (
    -- Warmup Task
    cpsFactorial, cpsFibonacci, cpsLength, cpsMap,
    cpsMergeSort, cpsSplit, cpsMerge,
    -- Main Task
    cpsEval
) where

-- You *may not* add imports from Data.Map, or any other imports
import qualified Data.Map (Map, lookup, insert, empty, fromList)
import A3Types (Env, emptyEnv, Value(..), HaskellProc(..), Expr(..))


------------------------------------------------------------------------------
-- * Warmup Task. CPS Transforming Haskell Functions *
------------------------------------------------------------------------------

-- | Compute the factorial of a number
-- factorial :: Int -> Int

-- | Compute the factorial of a number, in continuation passing style
cpsFactorial:: Int -> (Int -> r) -> r
cpsFactorial 0 k = k 1
cpsFactorial n k = cpsFactorial (n - 1) (\x -> k (x*n))

-- | Compute the n-th fibonacci number F(n).
--    Recall F(0) = 0, F(1) = 1, and F(n) = F(n-1) + F(n-2)

-- fibonacci :: Int -> Int
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

-- | Compute the n-th fibonacci number F(n), in continuation passing style
cpsFibonacci:: Int -> (Int -> r) -> r
cpsFibonacci 0 k = k 0
cpsFibonacci 1 k = k 1 
cpsFibonacci n k = cpsFibonacci (n - 1) (\v1 -> cpsFibonacci(n - 2) (\v2 -> k (v1 + v2)) )

------------------------------------------------------------------------------
-- | List functions

-- | CPS transform of the function `length`, which computes the length of a list
cpsLength :: [a] -> (Int -> r) -> r
cpsLength [] k = k 0
cpsLength [x] k = k 1
cpsLength (x:xs) k = cpsLength [x] (\v -> cpsLength (xs) (\v1 -> k (v + v1)))


-- | CPS transform of the function `map`. The argument function (to be applied
--   every element of the list) is written in direct style
cpsMap :: (a -> b) -> [a] -> ([b] -> r) -> r
cpsMap f [] k = k []
cpsMap f [x] k = k [f x]
cpsMap f (x:xs) k = cpsMap f [x] (\v -> cpsMap f (xs) (\v1 -> k (v ++ v1)))

------------------------------------------------------------------------------
-- Merge Sort

-- | Sort a list using mergeSort
-- mergeSort :: [Int] -> [Int]

-- | Split a list into two lists. All list elements in even indices
-- is placed in one sub-list, and all list elements in odd indicies
-- is placed in the second sub-list.
-- split :: [Int] -> ([Int], [Int])

-- | Merge two sorted lists together
-- merge :: [Int] -> [Int] -> [Int]

-- | CPS transform of mergeSort
cpsMergeSort :: [Int] -> ([Int] -> r) -> r
cpsMergeSort [] k = k []
cpsMergeSort [x] k = k [x]
cpsMergeSort xs k = cpsSplit xs (\res -> (cpsMergeSort (fst res) (\resy -> (cpsMergeSort (snd res) (\resf -> cpsMerge resy resf k)))))

-- | CPS transform of split
cpsSplit :: [Int] -> (([Int], [Int]) -> r) -> r
cpsSplit [] k = k ([],[])
cpsSplit [x] k = k ([x],[])
cpsSplit (x:y:xs) k = cpsSplit xs (\res -> k (x:(fst res), y:(snd res)))

cpsInsert :: [Int] -> Int -> ([Int] -> r) -> r
cpsInsert [] y k = k [y]
cpsInsert (x:xs) y k = if x > y
then k (y:x:xs)
else cpsInsert xs y (\res -> k (x:res))

-- | CPS transform of merge
cpsMerge :: [Int] -> [Int] -> ([Int] -> r) -> r
cpsMerge lst1 [] k = k lst1
cpsMerge [] lst2 k = k lst2
cpsMerge (x:xs) (y:ys) k = if x <= y then cpsMerge xs (y:ys) (\v -> k (x:v)) 
else cpsMerge (x:xs) ys (\v -> k (y:v)) 



------------------------------------------------------------------------------
-- * Main Task. CPS Transforming The StagShell Interpreter *
------------------------------------------------------------------------------

-- | A CPS interpreter `eval` for StagShell, which takes an environment,
--   an expression, and a continuation, and calls the continuation with
--   the evaluated value.
--   Notice that the type signature of `eval` is less general compared to
--   usual, i.e. it is not:
--      Env -> Expr -> (Value -> r) -> r
--   This restriction on the type of the continuation makes it easier
--   to check for errors.
cpsEval :: Env -> Expr -> (Value -> Value) -> Value
cpsEval env (Literal v) k = k v
cpsEval env (Lambda params body) k = k $ Closure $ Proc (\values k2 ->
    let paramArgTuples = zip params values
        newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                       env
                       paramArgTuples
    in cpsEval newEnv body k2)
cpsEval env (Plus a b) k = cpsEval env a (\res -> cpsEval env b (\resx -> case (res, resx) of 
    (Num x, Num y) -> k (Num (x + y))
    (Error a, _)   -> k (Error a)
    (_, Error b)   -> k (Error b)
    _              -> k (Error "Plus")))
cpsEval env (Times a b) k = cpsEval env a (\res -> cpsEval env b (\resx -> case (res, resx) of 
    (Num x, Num y) -> k (Num (x * y))
    (Error a, _)   -> k (Error a)
    (_, Error b)   -> k (Error b)
    (c, d)         -> k (Error "Times")))
cpsEval env (Equal a b) k = cpsEval env a (\res -> cpsEval env b (\resx -> case (res, resx) of 
    (Error c, _)   -> k (Error c)
    (_, Error d)   -> k (Error d)
    (c, d)         -> if c == d then T else F))
cpsEval env (Cons a b) k = cpsEval env a (\res -> cpsEval env b (\resx -> case (res, resx) of 
    (Error c, _)   -> k (Error c)
    (_, Error d)   -> k (Error d)
    (c, d)         -> k (Pair c d)))
cpsEval env (First expr) k = cpsEval env expr (\res -> case (res) of 
    (Pair a b) -> k a
    (Error c)  -> k (Error c)
    _          -> k (Error "First"))
cpsEval env (Rest expr) k = cpsEval env expr (\res -> case (res) of 
    (Pair a b) -> k b
    (Error c)  -> k (Error c)
    _          -> k (Error "Rest"))
cpsEval env (If cond expr alt) k = cpsEval env cond (\res -> case (res) of
    (Error c)  -> k (Error c)
    T          -> (cpsEval env expr k)
    _          -> (cpsEval env alt k))
cpsEval env (Var name) k = case (Data.Map.lookup name env) of 
    Just a  -> k a
    Nothing -> k (Error "Var")
cpsEval env (App proc args) k = cpsEval env proc (\res -> case (res) of 
    Closure (Proc f) ->  cpshelper args env k (\vargs -> cpsfoldl combineError Nothing vargs (\a -> 
                         case a of 
                             Just err -> err
                             Nothing  -> f vargs k))
                             -- note that we can no longer check if
                             -- the argument length is equal to the
                             -- parameter length, since "f" is opaque
    _                -> Error "App")
cpsEval env (Shift name expr) k = cpsEval (Data.Map.insert name (Closure $ Proc $ (\values k2 -> let cont = k in helperf values cont)) env) expr (\r -> r)
cpsEval env (Reset expr) k = if cpshelperShift expr then case (cpsEval env expr (\r -> r)) of 
    Error x -> Error x
    _ -> k (cpsEval env expr (\r -> r))
    else (cpsEval env expr k)

-- applies continuation on value
helperf :: [Value] -> (Value -> Value) -> Value
helperf [x] k = k x

-- checks for shift expression 
cpshelperShift :: Expr -> Bool 
cpshelperShift (Shift a b) = True 
cpshelperShift (Plus a b) = (cpshelperShift a) || (cpshelperShift b)
cpshelperShift (Times a b) = (cpshelperShift a)|| (cpshelperShift b)
cpshelperShift (Equal a b) = (cpshelperShift a) || (cpshelperShift b)
cpshelperShift (Cons a b) = (cpshelperShift a) || (cpshelperShift b)
cpshelperShift (First a) = (cpshelperShift a)
cpshelperShift (Rest a) = (cpshelperShift a)
cpshelperShift (If a b c) = (cpshelperShift b) || (cpshelperShift c)
cpshelperShift x = False

-- evaluates the list of arguments 
cpshelper :: [Expr] -> Env -> (Value -> Value) -> ([Value] -> r) -> r
cpshelper [] env k k1 = k1 []
cpshelper [x] env k k1 = k1 [cpsEval env x k]
cpshelper (x:xs) env k k1 = cpshelper [x] env k (\res -> cpshelper xs env k (\end -> k1 (res ++ end)))

cpsfoldl :: (b -> a -> b) -> b -> [a] -> (b -> r) -> r
cpsfoldl f acc [] k =  k acc
cpsfoldl f acc (x:xs) k =  cpsfoldl f (f acc x) xs k

-- Helper function that combines two (Maybe Value)s.
-- Returns `Nothing` if neither values are (Error _)
-- Returns the error if one of those values is (Error _)
combineError :: Maybe Value -> Value -> Maybe Value
combineError Nothing (Error v) = Just (Error v)
combineError acc     val       = acc