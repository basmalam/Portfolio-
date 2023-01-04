{-|
Module:        W06prac
Description:   Week 6 Practise Lab
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021

-}

-- This lists what this module exports. Don't change this!
module W07prac
  (
    mapMaybes,
    composeMaybe,
    foldMaybe, 
    applyBinaryMaybe,
    collectMaybes,
    Person(..),
    Robot(..),
    Organization(..),
    robotCompany
  )
where

-------------------------------------------------------------------------------
-- * Task 1: Practice with maybe
-------------------------------------------------------------------------------

mapMaybes :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybes f [] = []
mapMaybes f [Nothing] = [Nothing]
mapMaybes f [Just a] = [Just (f a)]
mapMaybes f (x:xs) = (mapMaybes f [x]) ++ (mapMaybes f xs)

composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe f g = \x -> case (f x) of   
  Nothing -> Nothing  
  otherwise -> (g (helper (f x)))    

foldMaybe :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybe f z [] = Just z
foldMaybe f z [a] = if (f z a) == Nothing then Nothing else (f z a)
foldMaybe f z (x:xs) = foldMaybe f (helper4 f z x) xs

helper4:: (b -> a -> b) -> x -> a -> b
helper4 f b x = (f b x)


applyBinaryMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyBinaryMaybe d Nothing b = Nothing 
applyBinaryMaybe d a Nothing = Nothing 
applyBinaryMaybe d (Just a) (Just b) = Just (d a b)

collectMaybes :: [Maybe a] -> Maybe [a]
collectMaybes [] = Just [] 
collectMaybes [Nothing] = Nothing 
collectMaybes [Just a] = Just [a] 
collectMaybes (x:xs) = if helper (x:xs) then Just (helper2 (x:xs)) else Nothing 

helper:: [Maybe a] -> Bool
helper [] = True
helper [Nothing] = False
helper [Just x] = True 
helper (x:xs) = (helper [x]) && helper xs

helper2:: [Maybe a] -> [a]
helper2 [] = []
helper2 [Just x] = [x]
helper2 [Nothing] = []
helper2 (x:xs) = (helper2 [x]) ++ (helper2 xs)
-------------------------------------------------------------------------------
-- * Task 2: Practice with Functor
-------------------------------------------------------------------------------

-- Here, we'll need to make Organization an instance of the Eq
-- *and* Functor typeclass.
data Person  = Person String Float      -- name, salary
             deriving (Show, Eq)
data Robot   = Robot Int                -- identifier
             deriving (Show, Eq)
data Organization p = Individual p             -- organization of one
                    | Team p [Organization p]  -- team leader, and list of sub-orgs
                    deriving (Show, Eq)
--helper for functor  
helper3 :: (a -> b) -> [Organization a] -> [Organization b]  
helper3 f [] = []   
helper3 f ((Individual p):xs) = (Individual (f p)):(helper3 f xs)  
helper3 f ((Team p ps):xs) = (Team (f p) (helper3 f ps)):(helper3 f xs)  

instance Functor Organization where
    fmap f (Individual p) = Individual (f p)
    fmap f (Team p ps) = Team (f p) (helper3 f ps)  


-- robot organization:
robot1   = Robot 1
robotOrg = Individual robot1

-- example:
owner   = Person "Janet" 100000
cto     = Person "Larry"  90000
cfo     = Person "Mike"   90000
intern  = Person "Sam"    40000
company = Team owner [Team cto [Individual intern],
                      Individual cfo]

-- Use a call to `fmap` to turn the example value `company`
-- into an organization with the same structure, but populated
-- entirely by robots. 
robotize :: Person -> Robot
robotize p = Robot 0

robotCompany = fmap robotize company
data Pizza b = Pepperoni ([b] -> b)
             | Cheese (b -> b -> b)
a = Cheese (\ x y -> True)

