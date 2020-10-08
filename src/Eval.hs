module Eval where

import LispVal
import Primitives

import Env
import Parser

import Control.Monad
import Control.Monad.Error
import System.IO

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@ (Atom _) : body)) = makeVarargs varargs env [] body
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List
                                                $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load",
                                              String (args !! 0)])) >>= hPutStrLn stderr

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
  ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
      then throwError $ NumArgs (num params) args
      else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
      remainingArgs = drop (length params) args
      num = toInteger.length
      evalBody env = liftM last $ mapM (eval env) body
      bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
        Nothing -> return env

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc x = case x of
  [func, List args] -> apply func args
  (func : args) -> apply func args

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
               ("open-input-file", makePort ReadMode),
               ("open-output-file", makePort WriteMode),
               ("close-input-port", closePort),
               ("close-output-port", closePort),
               ("read", readProc),
               ("write", writeProc),
               ("read-contents", readContents),
               ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort x = case x of
  [Port port] -> liftIO $ hClose port >> (return $ Bool True)
  _ -> return $ Bool False

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
