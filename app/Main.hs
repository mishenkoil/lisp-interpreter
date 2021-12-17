{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Exception
import Control.Monad
import Environement
import Evaluator
import Parser
import System.Environment
import System.Exit
import System.IO
import Tokenizer

main :: IO ()
main = do
  args <- getArgs
  let (interactive, args') = isInteractive args
  (env, last) <- loadFile args' getCoreFunc
  print last
  when interactive $ interactiveMode env
  exitSuccess

-- no file checking
loadFile :: [String] -> Env -> IO (Env, Expr)
loadFile [] _ = do
  putStrLn "Need a minimum of 1 arguement"
  exitWith (ExitFailure 84)
loadFile [file] env = do
  expr <- loadFile' file env
  return (evaluateFile env expr)
loadFile (hd : tl) env = do
  expr <- loadFile' hd env
  let (env', _) = evaluateFile env expr
  loadFile tl env'

loadFile' :: String -> Env -> IO [Expr]
loadFile' file env = do
  core <-
    catch
      (readFile file)
      ( \err -> do
          let err' = show (err :: IOException)
          putStrLn ("Can't open file: " ++ file ++ ": " ++ err')
          exitWith (ExitFailure 84)
      )
  return (parse $ tokenize core)

isInteractive :: [String] -> (Bool, [String])
isInteractive [] = (False, [])
isInteractive ("-i" : tl) = (True, tl)
isInteractive (hd : tl) = (inter, hd : args)
  where
    (inter, args) = isInteractive tl

interactiveMode :: Env -> IO ()
interactiveMode env = do
  putStr "> "
  hFlush stdout
  closed <- isEOF
  if closed
    then return ()
    else do
      line <- getLine
      let (env', res) = Evaluator.evaluate env $ head (parse $ tokenize line)
      print res
      interactiveMode env'

errorHandler :: String -> IO ()
errorHandler err = do
  putStrLn err
  exitWith (ExitFailure 84)
