{-|
Module: A2StarterTests
Description: Starter Tests for A2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}

module A2StartTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck) 

import A2 (runStag, eval)
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)


--Some simple tests to get you started--

prop_testLiteralNumber:: Int -> Property
prop_testLiteralNumber x = label "literal numbers" $
    let expr = (Literal $ Num x)
        result = runStag expr
    in result == Num x

prop_testAddition:: Int -> Int -> Property
prop_testAddition x y = label "addition tests" $
    let expr = (Plus (Literal $ Num x) (Literal $ Num y))
        result = runStag expr
    in result == Num (x + y)

prop_testTimes:: Int -> Int -> Property
prop_testTimes x y = label "times tests" $
    let expr = (Times (Literal $ Num x) (Literal $ Num y))
        result = runStag expr
    in result == Num (x * y)

prop_testTimes1:: Int -> Int -> Property
prop_testTimes1 x y = label "times tests1" $
    let expr = (Times (Literal $ Num 3) (Literal $ Num 3))
        result = runStag expr
    in result == Num 9

prop_testAdditionError:: Int -> Int -> Property
prop_testAdditionError x y = label "addition tests 1" $
    let expr = (Plus (Literal $ Num x) (Literal T))
        result = runStag expr
    in result == Error "Plus"
    
prop_testEqualAddition:: Int -> Int -> Property
prop_testEqualAddition x y = label "Equal tests" $
    let expr = (Equal (Plus (Literal $ Num x) (Literal $ Num y)) (Plus (Literal $ Num x) (Literal $ Num y)))
        result = runStag expr
    in result == T

prop_testVar:: Int -> Int -> Property
prop_testVar x y = label "Var" $
    let expr = (Var "a")
        result = runStag expr
    in result == Error "Var"


prop_testEqualAdditionEmbeded:: Int -> Int -> Property
prop_testEqualAdditionEmbeded x y = label "Equal tests embeded" $
    let expr = (Equal (Plus (Plus (Literal $ Num 1) (Literal $ Num 2)) (Literal $ Num 3)) (Plus (Literal $ Num 2) (Literal $ Num 4)))
        result = runStag expr
    in result == T

prop_testEqualAdditionError:: Int -> Int -> Property
prop_testEqualAdditionError x y = label "Equal tests Error " $
    let expr = (Equal (Plus (Literal T) (Literal $ Num 2)) (Plus (Literal $ Num 2) (Literal $ Num 3)))
        result = runStag expr
    in result == Error "Plus"

prop_testEqualTest:: Int -> Int -> Property
prop_testEqualTest x y = label "Equal tests 1" $
    let expr = (Equal (Literal T) (Literal $ Num 2))
        result = runStag expr
    in result == F

prop_testBasicIdentifier :: Property
prop_testBasicIdentifier = label "identifier error" $
  let expr = (Plus (Literal $ Num 3) (Times (Literal $ Num 3) (Literal T)))
      result = runStag expr
  in result == Error "Times"

prop_testConsBasic :: Int -> Int -> Property
prop_testConsBasic x y = label "cons test basic" $
  let expr = (Cons (Literal $ Num x) (Literal $ Num y) )
      result = runStag expr
  in result == Pair (Num x) (Num y)

prop_testConsBasic1 :: Int -> Int -> Int -> Property
prop_testConsBasic1 x y z = label "cons test basic1" $
  let expr = (Cons (Plus (Literal $ Num x) (Literal $ Num z)) (Literal $ Num y) )
      result = runStag expr
  in result == Pair (Num (x + z)) (Num y)

prop_testConsError :: Int -> Int -> Int -> Property
prop_testConsError x y z = label "cons test basic error" $
  let expr = (Cons (Plus (Literal T) (Literal $ Num z))  (Times (Literal T) (Literal $ Num z)) )
      result = runStag expr
  in result == Error "Plus"

prop_testFirst :: Int -> Property
prop_testFirst z = label "test first" $
  let expr = (First (Cons (Literal T) (Literal $ Num z) ))
      result = runStag expr
  in result == T

prop_testFirst1 :: Int -> Property
prop_testFirst1 z = label "test first1" $
  let expr = (First (Cons (Cons (Literal T) (Literal $ Num z) ) (Literal T)))
      result = runStag expr
  in result == Pair T (Num z)

prop_testRest :: Int -> Property
prop_testRest z = label "test rest" $
  let expr = (Rest (Cons (Literal T) (Literal $ Num z) ))
      result = runStag expr
  in result == (Num z)

prop_testIf :: Int -> Property
prop_testIf z = label "if test " $
  let expr = (If (Literal T) (Literal $ Num z) (Literal F) )
      result = runStag expr
  in result == (Num z)

prop_testFunctionApplication1 :: Int -> Int -> Property
prop_testFunctionApplication1 x y = label "function application1" $
    let fnExpr1 = Lambda ["x","y"] (Times (Var "x") (Var "y"))
        result = runStag (App fnExpr1 [(Literal (Num x)), (Literal (Num y))])
    in result == Num (x * y)

prop_testFunctionApplication11 :: Int -> Int -> Property
prop_testFunctionApplication11 x y = label "function application11" $
    let fnExpr1 = Lambda ["x","y"] (Plus (Times (Var "x") (Var "y")) (Literal $ Num x))
        result = runStag (App fnExpr1 [(Literal (Num x)), (Literal (Num y))])
    in result == Num ((x * y) + x)


prop_testFunctionApplication :: Int -> Int -> Property
prop_testFunctionApplication x y = label "function application" $
    let fnExpr1 = Lambda ["w"] (Plus (Literal (Num 1)) (Var "w"))
        fnExpr2 = Lambda ["a", "b"] (Times (Var "a") (App fnExpr1 [(Var "b")]))
        result = runStag (App fnExpr2 [(Literal (Num x)), (Literal (Num y))])
    in result == Num (x * (1 + y))
-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
    quickCheck prop_testLiteralNumber
    quickCheck prop_testAddition
    quickCheck prop_testAdditionError
    quickCheck prop_testEqualAddition
    quickCheck prop_testEqualAdditionEmbeded
    quickCheck prop_testEqualAdditionError
    quickCheck prop_testTimes
    quickCheck prop_testTimes1
    quickCheck prop_testEqualTest
    quickCheck prop_testConsBasic
    quickCheck prop_testConsBasic1
    quickCheck prop_testConsError
    quickCheck prop_testFirst
    quickCheck prop_testFirst1
    quickCheck prop_testRest
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testIf
    quickCheck prop_testVar
    quickCheck prop_testFunctionApplication1
    quickCheck prop_testFunctionApplication11
    quickCheck prop_testFunctionApplication
   
   