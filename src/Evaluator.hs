{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluator where

import Environement
import Parser

evaluate :: Env -> Expr -> (Env, Expr)
evaluate env (Quoted (Quoted expr)) = (env, Quoted expr)
evaluate env (Quoted expr@(Pair _ _)) = (env, expr')
  where
    expr' = case (map processPairs (pairToList expr)) of
      [] -> Nil
      x -> List x
evaluate env (Quoted expr) = (env, expr)
evaluate env lambda@(Lambda _ _) = (env, lambda)
evaluate env (Def var expr) = (env', Var var)
  where
    env' = addEnv env (var, expr)
evaluate env (Cond []) = (env, Nil)
evaluate env (Cond ((cond, expr) : tl)) = final
  where
    final
      | res = evaluate env' expr
      | otherwise = evaluate env' (Cond tl)
    (env', Bool res) = evaluate env cond
evaluate env (Func op args) = runFunc env'' op' args'
  where
    (env', op') = evaluate env op
    (env'', args') = evaluateList env' args
evaluate env (Let kvmap expr) = (env, res)
  where
    env' = generateLetEnv env kvmap
    (_, res) = evaluate env' expr
evaluate env (Var var) = (env, getVar env var)
evaluate env var = (env, var)

runFunc :: Env -> Expr -> [Expr] -> (Env, Expr)
runFunc env (Proc process) args = (env, process args)
runFunc env (Lambda params expr) args = (env, res)
  where
    env' = getFromEnv env params args
    (_, res) = evaluate env' expr
runFunc env _ _ = error "not procedur"

evaluateList :: Env -> [Expr] -> (Env, [Expr])
evaluateList env [] = (env, [])
evaluateList env (hd : tl) = (env'', res : next)
  where
    (env', res) = evaluate env hd
    (env'', next) = evaluateList env' tl

evaluateFile :: Env -> [Expr] -> (Env, Expr)
evaluateFile env [expr] = evaluate env expr
evaluateFile env (hd : tl) = next
  where
    (env', _) = evaluate env hd
    next = evaluateFile env' tl
