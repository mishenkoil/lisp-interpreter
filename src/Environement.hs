module Environement where

import Data.List (intercalate)
import Data.Map

data Expr
  = Nil -- parsed, eval tested
  | Num Int -- parsed, eval tested
  | Bool Bool -- parsed, eval tested
  | Var String -- parsed, eval tested
  | Quoted Expr -- parsed
  | Pair Expr Expr -- parsed
  | List [Expr] -- supporting Quoted type (done)
  | Def String Expr -- parsed, eval tested
  | Cond [(Expr, Expr)] -- parsed, eval tested
  | Lambda [Expr] Expr -- (parsed?), eval tested
  | Func Expr [Expr] -- parsed, eval tested
  | Proc ([Expr] -> Expr) -- (type inside evaluation), can't be tested
  | Let [(String, Expr)] Expr

instance Show Expr where
  show = showExpr

instance Eq Expr where
  Nil == Nil = True
  (Num n1) == (Num n2) = n1 == n2
  (Bool b1) == (Bool b2) = b1 == b2
  (Var s1) == (Var s2) = s1 == s2
  (Quoted q1) == (Quoted q2) = q1 == q2
  (Pair p1 p1') == (Pair p2 p2') = p1 == p2 && p1' == p2'
  (Def d1 d1') == (Def d2 d2') = d1 == d2 && d1' == d2'
  (Cond []) == (Cond []) = True
  (Cond ((c, e) : tl)) == (Cond ((c', e') : tl')) = c == c' && e == e' && Cond tl == Cond tl'
  (Lambda [] expr) == (Lambda [] expr') = expr == expr'
  (Lambda (hd : tl) expr) == (Lambda (hd' : tl') expr') = hd == hd' && Lambda tl expr == Lambda tl' expr'
  _ == _ = False

showExpr :: Expr -> String
showExpr Nil = "()"
showExpr (Num number) = show number
showExpr (Bool bool)
  | bool = "#t"
  | otherwise = "#f"
showExpr (Var variable) = variable
showExpr (Quoted e) = "'" ++ show e
showExpr (Func op args) = "(" ++ show op ++ concatMap ((\x -> " " ++ x) . show) args ++ ")"
showExpr pair@(Pair first rest)
  | isList pair = "(" ++ showLispList pair ++ ")"
  | otherwise = "(" ++ show first ++ " . " ++ show rest ++ ")"
showExpr (Lambda args func) = "#<procedure>"
showExpr (List values) = "(" ++ intercalate " " (Prelude.map show values) ++ ")"

isList :: Expr -> Bool
isList Nil = True
isList (Pair _ rest) = isList rest
isList _ = False

showLispList :: Expr -> String
showLispList Nil = []
showLispList (Pair first Nil) = show first
showLispList (Pair first rest) = show first ++ " " ++ showLispList rest

data Env
  = Empty
  | Env (Data.Map.Map String Expr) Env

instance Show Env where
  show = showEnv

instance Eq Env where
  Empty == Empty = True
  (Env maps next) == (Env maps' next') = toList maps == toList maps' && next == next'
  _ == _ = False

showEnv :: Env -> String
showEnv Empty = "Empty"
showEnv (Env maps next) = show (toList maps) ++ " " ++ show next

-- core functions utility
extractNum :: Expr -> Int
extractNum (Num n) = n
extractNum _ = error "can't extract Number from not Num type in arithmetic operation"

-- core function
fadd :: [Expr] -> Expr
fadd numbers = Num $ sum $ Prelude.map extractNum numbers

fsub :: [Expr] -> Expr
fsub [Num n] = Num (- n)
fsub (Num n : rest) = Num (n - (extractNum $ fadd rest))
fsub _ = error "not integer arguments for sub"

fmul :: [Expr] -> Expr
fmul numbers = Num $ Prelude.foldr (*) 1 (Prelude.map extractNum numbers)

fdiv :: [Expr] -> Expr
fdiv [Num n] = Num (1 `Prelude.div` n)
fdiv (Num n : rest) = case (extractNum $ fmul rest) of
  0 -> error "can't divide by 0"
  num -> Num $ quot n num
fdiv _ = error "not integer arguments for div"

fmod :: [Expr] -> Expr
fmod [Num n1, Num n2]
  | n2 == 0 = error "can't mod by 0"
  | otherwise = Num (n1 `Prelude.mod` n2)
fmod _ = error "need 2 number for mod"

lessThan :: [Expr] -> Expr
lessThan [Num n1, Num n2] = Bool (n1 < n2)
lessThan _ = error "need 2 number for <"

car :: [Expr] -> Expr
car [List values] = head values
car [Pair fst snd] = fst
car _ = error "car need list"

cdr :: [Expr] -> Expr
cdr [List values] = List (tail values)
cdr [Pair fst snd] = snd
cdr _ = error "cdr need list"

cons :: [Expr] -> Expr
cons [List hd1, List hd2] = List ((List hd1) : hd2)
cons [hd1, List hd2] = List (hd1 : hd2)
cons [hd1, hd2] = Pair hd1 hd2
cons _ = error "cons need 2 args"

equal :: [Expr] -> Expr
equal [Nil, Nil] = Bool True
equal [Num n1, Num n2] = Bool (n1 == n2)
equal [Var v1, Var v2] = Bool (v1 == v2)
equal [_, _] = Bool False
equal _ = error "eq? need 2 args"

atom :: [Expr] -> Expr
atom [Nil] = Bool True
atom [List _] = Bool False
atom (Quoted x : _) = Bool False
atom [Pair _ _] = Bool False
atom [_] = Bool True
atom _ = error "eq? need 1 args"

coreFunc =
  [ ("+", Proc fadd),
    ("-", Proc fsub),
    ("*", Proc fmul),
    ("div", Proc fdiv),
    ("mod", Proc fmod),
    ("<", Proc lessThan),
    ("cons", Proc cons),
    ("car", Proc car),
    ("cdr", Proc cdr),
    ("eq?", Proc equal),
    ("atom?", Proc atom)
  ]

getCoreFunc :: Env
getCoreFunc = Env (Data.Map.fromList coreFunc) Empty

generateLetEnv :: Env -> [(String, Expr)] -> Env
generateLetEnv env [] = env
generateLetEnv env (hd : tl) = addEnv (generateLetEnv env tl) hd

-- Environement gestion
addEnv :: Env -> (String, Expr) -> Env
addEnv (Env act next) (var, expr) = Env (Data.Map.insert var expr act) next

getFromEnv :: Env -> [Expr] -> [Expr] -> Env
getFromEnv env param args = Env (Data.Map.fromList (zip (Prelude.map show param) args)) env

getVar :: Env -> String -> Expr
getVar Empty _ = error "no var"
getVar (Env val next) var = case Data.Map.lookup var val of
  Just _var -> _var
  Nothing -> getVar next var

-- utility function for parser & evaluator
pairToList :: Expr -> [Expr]
pairToList Nil = []
pairToList (Pair hd tl) = hd : pairToList tl
pairToList x = [x]
