{-# LANGUAGE TemplateHaskell #-}

module Polyeme.Eval (eval, apply, isDatumType) where

import Control.Monad.Except
import Data.Maybe
import Polyeme.Datum

eval :: Environment -> Datum -> Result Datum
-- Atoms
eval env (Symbol name) = readEnv env name
eval ___ val@(Boolean _) = return val
eval ___ val@(Character _) = return val
eval ___ val@(Integer _) = return val
eval ___ val@(Real _) = return val
eval ___ val@(String _) = return val
eval ___ val@(Vector _) = do
  return val

-- Procedure literals
eval env (Pair (Symbol "lambda") (Pair paramsVal body)) = do
  params <- unpairParams paramsVal
  makeProc params body env

-- Special forms
eval ___ (Pair (Symbol "quote") val) = return val
eval env (Pair (Symbol "if") args) = do
  args <- unpairN 3 args
  cond <- eval env (head args)
  eval env (choice $ cond : tail args)
eval env (Pair (Symbol "cond") args) =
  cond env (unpair args)
eval env (Pair (Symbol "when") (Pair cond (Pair body _))) = do
  cond <- eval env cond
  if truthy cond
    then eval env body
    else return Void
eval env (Pair (Symbol "unless") (Pair cond (Pair body _))) = do
  cond <- eval env cond
  if truthy cond
    then return Void
    else eval env body
eval env (Pair (Symbol "case") args) = undefined
eval env (Pair (Symbol "let") (Pair bindings body)) = do
  env <- mapM validPair (unpair bindings) >>= extendEnv env
  mapM (eval env) (unpair body) >>= return . last
  where
    validPair :: Datum -> Result (String, Datum)
    validPair (Pair (Symbol name) (Pair val _)) = return (name, val)
    validPair other = throwError $ BadSpecialForm "Bad let bindings" other

-- Definitions
eval env (Pair (Symbol "def") (Pair (Symbol name) (Pair val _))) =
  eval env val >>= writeEnv env name >> return Void
eval env (Pair (Symbol "defn") (Pair (Symbol name) (Pair paramsVal body))) = do
  params <- unpairParams paramsVal
  procedure <- makeProc params body env
  writeEnv env name procedure
  return Void
eval env (Pair (Symbol "defmacro") (Pair (Symbol name) (Pair paramsVal body))) = do
  undefined

-- Function application
eval env (Pair setterVal@(Symbol setterName) (Pair varName@(Symbol _) argsVal))
  | last setterName == '!' = do
    setter <- eval env setterVal
    args <- mapM (eval env) (unpair argsVal)
    apply env setter (varName : args)
eval env (Pair funcVal argsVal) = do
  args <- mapM (eval env) (unpair argsVal)
  func <- eval env funcVal
  apply env func args

-- Bad syntax
eval ___ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Environment -> Datum -> [Datum] -> Result Datum
apply env (Function f) args = f env args
apply ___ (Procedure params varParam body closure) args
  | length params /= length args && varParam == Nothing =
    throwError $ NumArgs (length params) args
  | otherwise = case varParam of
    Nothing -> extendEnv closure (zip params args) >>= evalBody
    Just varParam -> do
      let (paramArgs, varargs) = splitAt (length params) args
      let bindings = (varParam, pair varargs) : (zip params paramArgs)
      extendEnv closure bindings >>= evalBody
  where
    evalBody env = mapM (eval env) (unpair body) >>= return . last
apply ___ val _ = throwError $ NotFunction "Not a function" (show val)

unpairN :: Int -> Datum -> Result [Datum]
unpairN n pair = do
  let args = unpair pair
  if length args /= n
    then throwError $ NumArgs n args
    else return args

unpairParams :: Datum -> Result [String]
unpairParams pair = mapM getSymbol (unpair pair)
  where
    getSymbol :: Datum -> Result String
    getSymbol (Symbol sym) = return sym
    getSymbol _ = throwError $ Default "Expecting argument name"

makeProc :: [String] -> Datum -> Environment -> Result Datum
makeProc params body closure =
  if length params > 0 && last params == ".."
    then return $ Procedure (init $ init params) (Just $ last $ init params) body closure
    else return $ Procedure params Nothing body closure

choice :: [Datum] -> Datum
choice [(Boolean False), _, alt] = alt
choice [_, consq, _] = consq

cond :: Environment -> [Datum] -> Result Datum
cond env [(Symbol "else"), consq] = eval env consq
cond env (pred : consq : xs) = do
  result <- eval env pred
  if truthy result
    then eval env consq
    else cond env xs
cond _ [_] = throwError $ Default "Bad cond form"
cond _ [] = throwError $ Default "No matching cases in cond"

truthy :: Datum -> Bool
truthy (Boolean False) = False
truthy _ = True

isDatumType constr =
  [|
    unaryFunc
      ( \x -> return $
          Boolean $ case x of
            $(constr) -> True
            _ -> False
      )
    |]
