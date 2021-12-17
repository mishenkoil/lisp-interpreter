import Environement_Test
import Evaluator_Test
import Test.HUnit

main :: IO Counts
main = do
  print "[Environement_Test]:"
  runTestTT $
    TestList
      [ test_add,
        test_sub,
        test_mul,
        test_div,
        test_mod,
        test_lessThan,
        test_car,
        test_cdr,
        test_equal_numberTrue,
        test_equal_numberFalse,
        test_equal_pairTrue,
        test_equal_pairFalse,
        test_atom_pairEmpty,
        test_atom_pair,
        test_atom_number
      ]
  print "[Evaluator_Test]:"
  runTestTT $
    TestList
      [ test_evaluator_var,
        test_evaluator_number,
        test_evaluator_pairEmpty,
        test_evaluator_pairOneVal,
        test_evaluator_pairMulVal,
        test_evaluator_pairList,
        test_evaluator_bool,
        test_evaluator_cons,
        test_evaluator_define,
        test_evaluator_condFst,
        test_evaluator_condSnd,
        test_evaluator_lambdaParam,
        test_evaluator_lambdaNoParam
      ]
