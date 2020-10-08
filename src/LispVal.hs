module LispVal where

import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

instance Show LispVal where
  show (String x) = "\"" ++ x ++ "\""
  show (Atom x) = x
  show (Number x) = show x
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List x) = "(" ++ unwordsList x ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func { params = args, vararg = varargs, body = body, closure = env }) =
    "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
  show (Port _) = "<IO port>"
  show (IOFunc _) = "<IO primitive>"

data LispError = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ func
  show (NumArgs expected found) = "Expected " ++ show expected
  show (TypeMismatch expected found) = "Invalid type: " ++ show found ++ " expected " ++ expected
  show (Parser err) = "Parse error at " ++ show err

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows x = case x of
  (Left err) -> throwError err
  (Right val) -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
