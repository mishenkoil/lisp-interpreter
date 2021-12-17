module Evaluator_Test where

import Data.Map
import Environement
import Evaluator
import Test.HUnit

test_evaluator_var :: Test
test_evaluator_var =
  TestCase $
    assertEqual
      "[evaluator]: simple variale"
      (Num 1)
      (snd $ evaluate env (Var "var"))
  where
    env = addEnv getCoreFunc ("var", Num 1)

test_evaluator_number :: Test
test_evaluator_number =
  TestCase $
    assertEqual
      "[evaluator]: simple number"
      (Num 1)
      (snd $ evaluate getCoreFunc (Num 1))

test_evaluator_pairEmpty :: Test
test_evaluator_pairEmpty =
  TestCase $
    assertEqual
      "[evaluator]: simple empty pair"
      (Pair Nil Nil)
      (snd $ evaluate getCoreFunc (Pair Nil Nil))

test_evaluator_pairOneVal :: Test
test_evaluator_pairOneVal =
  TestCase $
    assertEqual
      "[evaluator]: simple pair with 1 value"
      (Pair (Num 1) Nil)
      (snd $ evaluate getCoreFunc (Pair (Num 1) Nil))

test_evaluator_pairMulVal :: Test
test_evaluator_pairMulVal =
  TestCase $
    assertEqual
      "[evaluator]: simple pair with 1 value"
      (Pair (Num 1) (Pair (Num 2) Nil))
      (snd $ evaluate getCoreFunc (Pair (Num 1) (Pair (Num 2) Nil)))

test_evaluator_pairList :: Test
test_evaluator_pairList =
  TestCase $
    assertEqual
      "[evaluator]: simple list with pair"
      (Pair (Num 1) (Pair (Num 2) Nil))
      (snd $ evaluate getCoreFunc (Pair (Num 1) (Pair (Num 2) Nil)))

test_evaluator_bool :: Test
test_evaluator_bool =
  TestCase $
    assertEqual
      "[evaluator]: simple boolean"
      (Bool True)
      (snd $ evaluate getCoreFunc (Bool True))

test_evaluator_cons :: Test
test_evaluator_cons =
  TestCase $
    assertEqual
      "[evaluator]: cons function"
      (Pair (Num 1) (Pair (Num 2) Nil))
      (snd $ evaluate getCoreFunc (Func (Var "cons") [Num 1, Func (Var "cons") [Num 2, Nil]]))

test_evaluator_define :: Test
test_evaluator_define =
  TestCase $
    assertEqual
      "[evaluator]: define"
      (Env (Data.Map.fromList [("var", (Num 1))]) Empty, Num 1)
      (evaluate Empty (Def "var" (Num 1)))

test_evaluator_condFst :: Test
test_evaluator_condFst =
  TestCase $
    assertEqual
      "[evaluator]: first condition is true"
      (Num 1)
      (snd $ evaluate Empty (Cond [(Bool True, Num 1), (Bool False, Num 2)]))

test_evaluator_condSnd :: Test
test_evaluator_condSnd =
  TestCase $
    assertEqual
      "[evaluator]: second condition is true"
      (Num 2)
      (snd $ evaluate Empty (Cond [(Bool False, Num 1), (Bool True, Num 2)]))

test_evaluator_lambdaParam :: Test
test_evaluator_lambdaParam =
  TestCase $
    assertEqual
      "[evaluator]: lambda with parameters"
      (Num 3)
      (snd $ evaluate getCoreFunc (Func (Lambda [Var "a", Var "b"] (Func (Var "+") [Var "a", Var "b"])) [Num 1, Num 2]))

test_evaluator_lambdaNoParam :: Test
test_evaluator_lambdaNoParam =
  TestCase $
    assertEqual
      "[evaluator]: lambda with out parameters"
      (Num 3)
      (snd $ evaluate getCoreFunc (Func (Lambda [] (Func (Var "+") [Num 1, Num 2])) []))
