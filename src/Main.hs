module Main where

import Eval
import Repl

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne $ args
