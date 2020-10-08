module Primitives where

import LispVal

import Control.Monad.Error
import Data.IORef
import System.IO

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainer", numericBinop rem),
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  ("/=", numBoolBinop (/=)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("string=?", strBoolBinop (==)),
  ("string?", strBoolBinop (>)),
  ("string<=?", strBoolBinop (<=)),
  ("string>=?", strBoolBinop (>=)),
  ("car", car),
  ("cdr", cdr),
  ("const", cons)]


car :: [LispVal] -> ThrowsError LispVal
car c = case c of
  [List (x:xs)] -> return x
  [DottedList (x:xs) _] -> return x
  [badArg] -> throwError $ TypeMismatch "pair" badArg
  badArgList -> throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr c = case c of
  [List (x:xs)] -> return $ List xs
  [DottedList (_:xs) x] -> return $ DottedList xs x
  [DottedList (xs) x] -> return x
  [badArg] -> throwError $ TypeMismatch "pair" badArg
  badArgList -> throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons c = case c of
  [x, List []] -> return $ List [x]
  [x, List xs] -> return $ List $ [x] ++ xs
  [x, DottedList xs xl] -> return $ DottedList ([x] ++ xs) xl
  [x1, x2] -> return $ DottedList [x1] x2
  badArgList -> throwError $ NumArgs 2 badArgList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@ [_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum x = case x of
  (Number n) -> return n
  (String n) -> let parsed = reads n in
    if null parsed
    then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ parsed !! 0
  (List [n]) -> unpackNum n
  notNum -> throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr x = case x of
  (String s) -> return s
  (Number s) -> return $ show s
  (Bool s) -> return $ show s
  notString -> throwError $ TypeMismatch "string" notString
  

unpackBool :: LispVal -> ThrowsError Bool
unpackBool x = case x of
  (Bool b) -> return b
  notBool -> throwError $ TypeMismatch "boolean" notBool
