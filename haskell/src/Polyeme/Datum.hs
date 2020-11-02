module Polyeme.Datum
  ( Datum (..),
    Error (..),
    Result,
    Environment,
    pair,
    unpair,
    readEnv,
    writeEnv,
    mutateEnv,
    extendEnv,
  )
where

import Control.Monad.Except
import qualified Data.HashMap.Strict as H
import Data.IORef
import qualified Data.Vector as V
import System.IO
import Text.ParserCombinators.Parsec

data Datum
  = Void
  | Symbol String
  | Boolean Bool
  | Character Char
  | Integer Int
  | Real Double
  | String String
  | Vector (V.Vector Datum)
  | Hash (H.HashMap String Datum)
  | Port Handle
  | Pair Datum Datum
  | Environment Environment
  | Function (Environment -> [Datum] -> Result Datum)
  | Procedure
      { params :: [String],
        varParam :: Maybe String,
        body :: Datum,
        closure :: Environment
      }

data Error
  = NumArgs Int [Datum]
  | TypeMismatch String Datum
  | Parser ParseError
  | BadSpecialForm String Datum
  | NotFunction String String
  | UnboundVar String String
  | Default String

type Environment = IORef [(String, IORef Datum)]

type Result = ExceptT Error IO

instance Show Datum where
  show (Void) = "#<void>"
  show (Symbol symbl) = symbl
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Character char) = '#' : '\'' : char : []
  show (Integer number) = show number
  show (Real number) = show number
  show (String contents) = "\"" ++ contents ++ "\""
  show (Hash _) = "#<hash>"
  show (Port _) = "#<port>"
  show pair@(Pair _ _) = "(" ++ (unwords . map show . unpair) pair ++ ")"
  show (Vector contents) = "#(" ++ (unwords . V.toList . V.map show) contents ++ ")"
  show (Environment _) = "#<environment>"
  show (Function _) = "#<native-proc>"
  show (Procedure {}) = "#<procedure>"

instance Show Error where
  show (NumArgs expected got) =
    "Expected " ++ show expected ++ " arguments, instead got "
      ++ (show $ length got)
      ++ " values: "
      ++ (unwords . map show) got
  show (TypeMismatch expected got) =
    "Expected type " ++ expected ++ ", instead got: " ++ show got
  show (Parser parseErr) = show parseErr
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message funcname) = message ++ ": " ++ funcname
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (Default message) = message

pair :: [Datum] -> Datum
pair (x : xs) = Pair x (pair xs)
pair [] = Symbol "()"

unpair :: Datum -> [Datum]
unpair (Pair x y) = x : unpair y
unpair (Symbol "()") = []
unpair other = [other]

readEnv :: Environment -> String -> Result Datum
readEnv envRef name = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Variable name is undefined" name)
    (liftIO . readIORef)
    (lookup name env)

writeEnv :: Environment -> String -> Datum -> Result Datum
writeEnv envRef name value = do
  env <- liftIO $ readIORef envRef
  if isDefined env name
    then mutateEnv envRef name value
    else do
      valRef <- liftIO $ newIORef value
      liftIO $ writeIORef envRef ((name, valRef) : env)
      return value
  where
    isDefined env name = maybe False (const True) (lookup name env)

mutateEnv :: Environment -> String -> Datum -> Result Datum
mutateEnv envRef name value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Variable name is undefined" name)
    (liftIO . (`writeIORef` value))
    (lookup name env)
  return value

-- ! ERROR ! old env gets modified, extend should return new env
extendEnv :: Environment -> [(String, Datum)] -> Result Environment
extendEnv envRef bindings = do
  newEnv <- liftIO $ readIORef envRef
  newEnvRef <- liftIO $ newIORef newEnv
  mapM (uncurry $ writeEnv newEnvRef) bindings
  return newEnvRef
