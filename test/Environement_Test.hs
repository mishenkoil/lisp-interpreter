module Environement_Test where

import Data.Map
import Environement
import Evaluator
import Test.HUnit

test_add :: Test
test_add =
  TestCase $
    assertEqual
      "[add]: simple add of two number"
      (Num 3)
      (snd $ evaluate getCoreFunc (Func (Var "+") [Num 1, Num 2]))

test_sub :: Test
test_sub =
  TestCase $
    assertEqual
      "[sub]: simple sub of two number"
      (Num 1)
      (snd $ evaluate getCoreFunc (Func (Var "-") [Num 2, Num 1]))

test_mul :: Test
test_mul =
  TestCase $
    assertEqual
      "[mul]: simple mul of two number"
      (Num 4)
      (snd $ evaluate getCoreFunc (Func (Var "*") [Num 2, Num 2]))

test_div :: Test
test_div =
  TestCase $
    assertEqual
      "[div]: simple div of two number"
      (Num 2)
      (snd $ evaluate getCoreFunc (Func (Var "div") [Num 2, Num 1]))

test_mod :: Test
test_mod =
  TestCase $
    assertEqual
      "[mod]: simple mod of two number"
      (Num 1)
      (snd $ evaluate getCoreFunc (Func (Var "mod") [Num 3, Num 2]))

test_lessThan :: Test
test_lessThan =
  TestCase $
    assertEqual
      "[lessThan]: simple less than between two number"
      (Bool False)
      (snd $ evaluate getCoreFunc (Func (Var "<") [Num 3, Num 2]))

test_car :: Test
test_car =
  TestCase $
    assertEqual
      "[car]: car on a list"
      (Num 1)
      (snd $ evaluate getCoreFunc (Func (Var "car") [Pair (Num 1) Nil]))

test_cdr :: Test
test_cdr =
  TestCase $
    assertEqual
      "[cdr]: cdr on a list"
      (Pair (Num 2) Nil)
      (snd $ evaluate getCoreFunc (Func (Var "cdr") [Pair (Num 1) (Pair (Num 2) Nil)]))

test_equal_numberTrue :: Test
test_equal_numberTrue =
  TestCase $
    assertEqual
      "[equal]: equal on number, that return true"
      (Bool True)
      (snd $ evaluate getCoreFunc (Func (Var "eq?") [Num 1, Num 1]))

test_equal_numberFalse :: Test
test_equal_numberFalse =
  TestCase $
    assertEqual
      "[equal]: equal on number, that return false"
      (Bool False)
      (snd $ evaluate getCoreFunc (Func (Var "eq?") [Num 2, Num 1]))

test_equal_pairTrue :: Test
test_equal_pairTrue =
  TestCase $
    assertEqual
      "[equal]: equal on pair, that return true"
      (Bool True)
      (snd $ evaluate getCoreFunc (Func (Var "eq?") [Pair Nil Nil, Pair Nil Nil]))

test_equal_pairFalse :: Test
test_equal_pairFalse =
  TestCase $
    assertEqual
      "[equal]: equal on pair (empty), that return false"
      (Bool False)
      (snd $ evaluate getCoreFunc (Func (Var "eq?") [Pair (Num 1) Nil, Pair (Num 1) Nil]))

test_atom_pairEmpty :: Test
test_atom_pairEmpty =
  TestCase $
    assertEqual
      "[atom]: atom on empty pair"
      (Bool True)
      (snd $ evaluate getCoreFunc (Func (Var "atom?") [Pair Nil Nil]))

test_atom_pair :: Test
test_atom_pair =
  TestCase $
    assertEqual
      "[atom]: atom on pair"
      (Bool False)
      (snd $ evaluate getCoreFunc (Func (Var "atom?") [Pair (Num 1) Nil]))

test_atom_number :: Test
test_atom_number =
  TestCase $
    assertEqual
      "[atom]: atom on number"
      (Bool True)
      (snd $ evaluate getCoreFunc (Func (Var "atom?") [Num 1]))
