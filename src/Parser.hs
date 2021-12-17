module Parser where

import Environement
import Tokenizer

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapScd :: (b -> c) -> (a, b) -> (a, c)
mapScd f (a, b) = (a, f b)

parse :: [Token] -> [Expr]
parse [] = []
parse tokens = (processPairs expr) : parse rest
  where
    (expr, rest) = parse' tokens

parse' :: [Token] -> (Expr, [Token])
parse' [] = (Nil, [])
parse' (Quote : OpenP : CloseP : xs) = (Quoted Nil, xs)
parse' (OpenP : xs) = parseSeveral xs
parse' (Number num : xs) = (Num (read num :: Int), xs)
parse' (Word "#t" : xs) = (Bool True, xs)
parse' (Word "#f" : xs) = (Bool False, xs)
parse' (Word x : xs) = (Var x, xs)
parse' (Quote : xs) = mapFst Quoted $ parse' xs
parse' _ = error "Unknown token"

parseSeveral :: [Token] -> (Expr, [Token])
parseSeveral [] = error "no closing parenthesis"
parseSeveral (CloseP : xs) = error "has empty expression"
parseSeveral tokens = parseSeveral' tokens

parseSeveral' :: [Token] -> (Expr, [Token])
parseSeveral' [] = (Nil, [])
parseSeveral' (CloseP : xs) = (Nil, xs)
parseSeveral' tokens = parseSeveral'' tokens

parseSeveral'' :: [Token] -> (Expr, [Token])
parseSeveral'' tokens = (\(x, (y, z)) -> (Pair x y, z)) $ mapScd parseSeveral' (parse' tokens)

processPairs :: Expr -> Expr
processPairs Nil = Nil
processPairs (Num num) = Num num
processPairs (Bool bool) = Bool bool
processPairs (Var value) = Var value
processPairs (Pair (Var "define") rest) = createDef rest
processPairs (Pair (Var "quote") (Pair expr rest)) = Quoted expr
processPairs (Pair (Var "cond") rest) = createCond rest
processPairs (Pair (Var "lambda") rest) = createLambda rest
processPairs (Pair (Var "let") rest) = createLet rest
processPairs (Quoted expr) = Quoted expr
processPairs (Pair op args) = Func (processPairs op) (map processPairs (pairToList args))
processPairs _ = error "Unknown grammar"

createDef :: Expr -> Expr
createDef (Pair (Var name) (Pair rest Nil)) = Def name (processPairs rest)
createDef (Pair (Pair (Var name) param) (Pair rest Nil)) = Def name (createLambda (Pair param (Pair rest Nil)))
createDef _ = error "def creation failed"

createCond :: Expr -> Expr
createCond Nil = Cond []
createCond (Pair (Pair boolFunc (Pair expr Nil)) rest) =
  Cond ((processPairs boolFunc, processPairs expr) : conds)
  where
    (Cond conds) = createCond rest
createCond _ = error "cond creation failed"

createLambda :: Expr -> Expr
createLambda (Pair param (Pair rest Nil)) = Lambda (pairToList param) (processPairs rest)
createLambda _ = error "lambda creation failed"

createLet :: Expr -> Expr
createLet (Pair param (Pair rest Nil)) = Let (letParamList param) (processPairs rest)
createLet _ = error "let creation failed"

letParamList :: Expr -> [(String, Expr)]
letParamList Nil = []
letParamList (Pair (Var var) (Pair expr Nil)) = [(var, processPairs expr)]
letParamList (Pair (Pair (Var var) (Pair expr Nil)) next) = (var, processPairs expr) : letParamList next
letParamList _ = error "letParamList creation failed"
