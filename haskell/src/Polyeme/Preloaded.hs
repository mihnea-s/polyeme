{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Polyeme.Preloaded (preload) where

import Control.Arrow
import Control.Monad.Except
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Maybe
import qualified Data.Vector as V
import Polyeme.Datum
import Polyeme.Eval
import Polyeme.Parser
import System.IO
import System.Process

preload :: Environment -> Result Environment
preload env = extendEnv env (map (second Function) preloadedFuncs)

preloadedFuncs :: [(String, Environment -> [Datum] -> Result Datum)]
preloadedFuncs =
  [ ----------------
    -- Primitives --
    ----------------
    ("set!", setFunc),
    ("exit", exitFunc),
    ("load", loadFunc),
    ("eval", evalFunc),
    ("apply", applyFunc),
    ("system", systemFunc),
    ("void", makeDatum (\_ -> Void) ()),
    -----------------------
    -- Boolean functions --
    -----------------------
    ("and", opBoolBoolBool (&&)),
    ("or", opBoolBoolBool (||)),
    -------------------------
    -- Character functions --
    -------------------------
    ("chr=?", opChrChrBool (==)),
    ("chr>?", opChrChrBool (>)),
    ("chr<?", opChrChrBool (<)),
    ("chr>=?", opChrChrBool (>=)),
    ("chr<=?", opChrChrBool (<=)),
    ("chr-digit?", opChrBool (isNumber)),
    ("chr-alpha?", opChrBool (isAlpha)),
    ("chr-alnum?", opChrBool (isAlphaNum)),
    ("chr-space?", opChrBool (isSpace)),
    ("chr-lower?", opChrBool (isLower)),
    ("chr-upper?", opChrBool (isUpper)),
    -----------------------
    -- Numeric functions --
    -----------------------
    ("-", opNumNumNum (-)),
    ("+", opNumNumNum (+)),
    ("*", opNumNumNum (*)),
    ("/", numDivision AsReal),
    ("//", numDivision AsInt),
    ("mod", numDivision AsReminder),
    ("=?", opNumNumBool (==)),
    ("<?", opNumNumBool (<)),
    (">?", opNumNumBool (>)),
    ("/=?", opNumNumBool (/=)),
    (">=?", opNumNumBool (>=)),
    ("<=?", opNumNumBool (<=)),
    ("ceil", opRealReal (ceiling)),
    ("floor", opRealReal (floor)),
    ("trunc", opRealReal (truncate)),
    ("round", opRealReal (round)),
    ("exp", opRealReal (exp)),
    ("log", opRealReal (log)),
    ("sin", opRealReal (sin)),
    ("cos", opRealReal (cos)),
    ("tan", opRealReal (tan)),
    ("asin", opRealReal (asin)),
    ("acos", opRealReal (acos)),
    ("atan", opRealReal (atan)),
    ----------------------
    -- String functions --
    ----------------------
    ("str", makeDatum (String) ""),
    ("str@", containerAt StrCtner),
    ("str+", opStrStrStr (++)),
    ("str-nl", strStringNewline),
    ("str-len", containerLen StrCtner),
    ("str-sub", strSubstring),
    ("str-set!", containerSet StrCtner),
    ("str-fill!", containerFill StrCtner),
    ("str=?", opStrStrBool (==)),
    ("str<?", opStrStrBool (<)),
    ("str>?", opStrStrBool (>)),
    ("str<=?", opStrStrBool (<=)),
    ("str>=?", opStrStrBool (>=)),
    ----------------------
    -- Vector functions --
    ----------------------
    ("vec", makeDatum (Vector) V.empty),
    ("vec@", containerAt VecCtner),
    ("vec-len", containerLen VecCtner),
    ("vec-set!", containerSet VecCtner),
    ("vec-fill!", containerFill VecCtner),
    --------------------
    -- Hash functions --
    --------------------
    ("hash", makeDatum (Hash) H.empty),
    ("hash@", containerAt HashCtner),
    ("hash-set!", containerSet HashCtner),
    ("hash-keys", hashElems (map (String) . H.keys)),
    ("hash-vals", hashElems H.elems),
    --------------------
    -- Pair functions --
    --------------------
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("nil?", isNil),
    ("list", const $ return . pair),
    ------------------
    -- IO functions --
    ------------------
    ("read", readProc),
    ("write", writeProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read-contents", readContents),
    ---------------------------
    -- Polymorphic functions --
    ---------------------------
    ("eq?", isEq),
    ("same?", isSame),
    ("sym?", isSym),
    ("bool?", isBool),
    ("chr?", isChr),
    ("int?", isInt),
    ("real?", isReal),
    ("str?", isStr),
    ("vec?", isVec),
    ("hash?", isHash),
    ("port?", isPort),
    ("list?", isList),
    ("env?", isEnv),
    ("proc?", isProc),
    ----------------------------
    -- Type casting functions --
    ----------------------------
    ("sym->str", unaryFunc symToStr),
    ("bool->str", unaryFunc boolToStr),
    ("chr->str", unaryFunc chrToStr),
    ("chr->int", unaryFunc chrToInt),
    ("int->chr", unaryFunc intToChr),
    ("num->str", unaryFunc numToStr),
    ("str->sym", unaryFunc strToSym),
    ("str->int", unaryFunc strToInt),
    ("str->real", unaryFunc strToReal),
    ("str->vec", unaryFunc strToVec),
    ("vec->str", unaryFunc vecToStr),
    ("vec->list", unaryFunc vecToList),
    ("hash->vec", unaryFunc hashToVec),
    ("hash->env", unaryFunc hashToEnv)
  ]

-----------------------
-- Utility functions --
-----------------------

makeDatum :: (a -> Datum) -> a -> Environment -> [Datum] -> Result Datum
makeDatum func defaultVal = constFunc (\_ -> return $ func defaultVal)

constFunc :: (() -> Result Datum) -> Environment -> [Datum] -> Result Datum
constFunc func _ [] = func ()
constFunc _ _ args = throwError $ NumArgs 0 args

unaryFunc :: (Datum -> Result Datum) -> Environment -> [Datum] -> Result Datum
unaryFunc func _ [arg] = func arg
unaryFunc _ _ args = throwError $ NumArgs 1 args

binaryFunc :: (Datum -> Datum -> Result Datum) -> Environment -> [Datum] -> Result Datum
binaryFunc func _ [x, y] = func x y
binaryFunc _ _ args = throwError $ NumArgs 2 args

ternaryFunc :: (Datum -> Datum -> Datum -> Result Datum) -> Environment -> [Datum] -> Result Datum
ternaryFunc func _ [x, y, z] = func x y z
ternaryFunc _ _ args = throwError $ NumArgs 3 args

-------------------------
-- Primitive functions --
-------------------------

setFunc :: Environment -> [Datum] -> Result Datum
setFunc env [Symbol name, value] = mutateEnv env name value
setFunc _ args = throwError $ TypeMismatch "name, value" (pair args)

exitFunc :: Environment -> [Datum] -> Result Datum
exitFunc _ [String m] = throwError $ Default $ "Exit was called: " ++ m
exitFunc _ [value] = throwError $ Default $ "Exit was called: " ++ show value
exitFunc _ _ = throwError $ Default $ "Exit was called, program terminated"

loadFunc :: Environment -> [Datum] -> Result Datum
loadFunc env [(String fname)] = do
  contents <- liftIO $ readFile fname
  parsed <- readExprs contents fname
  results <- mapM (eval env) parsed
  return $ pair results
loadFunc _ args = throwError $ TypeMismatch "filename" (pair args)

applyFunc :: Environment -> [Datum] -> Result Datum
applyFunc env (x : xs) = apply env x xs
applyFunc ___ [] = throwError $ NumArgs 1 []

evalFunc :: Environment -> [Datum] -> Result Datum
evalFunc = binaryFunc func
  where
    func :: Datum -> Datum -> Result Datum
    func exp (Environment env) = eval env exp
    func _ arg = throwError $ TypeMismatch "environment" arg

systemFunc :: Environment -> [Datum] -> Result Datum
systemFunc _ ((String cmd) : args) =
  -- ! Invalid inputs are silently ignored
  let strArgs = mapMaybe (\x -> case x of String arg -> Just arg; _ -> Nothing) args
   in if length args /= length strArgs
        then throwError $ TypeMismatch "list of strings" (pair args)
        else createProc cmd strArgs
  where
    createProc :: String -> [String] -> Result Datum
    createProc cmd args = do
      let processArgs =
            (proc cmd args)
              { std_in = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe
              }
      pid <- liftIO $ createProcess processArgs
      case pid of
        (Just hin, Just hout, _, _) -> return $ Pair (Port hin) (Port hout)
        _ -> throwError $ Default "error creating process"
systemFunc _ (arg : _) = throwError $ TypeMismatch "command" arg
systemFunc _ [] = throwError $ NumArgs 1 []

-----------------------
-- Boolean functions --
-----------------------

opBoolBoolBool :: (Bool -> Bool -> Bool) -> Environment -> [Datum] -> Result Datum
opBoolBoolBool op = binaryFunc func
  where
    func :: Datum -> Datum -> Result Datum
    func (Boolean b1) (Boolean b2) = return $ Boolean $ op b1 b2
    func x y = throwError $ TypeMismatch "boolean" (Pair x y)

-------------------------
-- Character functions --
-------------------------

opChrBool :: (Char -> Bool) -> Environment -> [Datum] -> Result Datum
opChrBool op = unaryFunc func
  where
    func :: Datum -> Result Datum
    func (Character c) = return $ Boolean $ op c
    func x = throwError $ TypeMismatch "character" x

opChrChrBool :: (Char -> Char -> Bool) -> Environment -> [Datum] -> Result Datum
opChrChrBool op = binaryFunc func
  where
    func :: Datum -> Datum -> Result Datum
    func (Character c1) (Character c2) = return $ Boolean $ op c1 c2
    func x y = throwError $ TypeMismatch "character" (Pair x y)

------------------------
-- Numberic functions --
------------------------

opRealReal :: Real a => (Double -> a) -> Environment -> [Datum] -> Result Datum
opRealReal op = unaryFunc func
  where
    func :: Datum -> Result Datum
    func (Integer a) = func (Real $ fromIntegral a)
    func (Real a) = return $ Real $ fromRational . toRational $ op a
    func other = throwError $ TypeMismatch "number" other

opNumNumNum :: (forall n. Num n => n -> n -> n) -> Environment -> [Datum] -> Result Datum
opNumNumNum op = binaryFunc func
  where
    func (Integer a) (Integer b) = return $ Integer (op a b)
    func (Real a) (Real b) = return $ Real (op a b)
    func (Integer a) b = func (Real $ fromIntegral a) b
    func a (Integer b) = func a (Real $ fromIntegral b)
    func x y = throwError $ TypeMismatch "number" (Pair x y)

opNumNumBool :: (forall n. Ord n => n -> n -> Bool) -> Environment -> [Datum] -> Result Datum
opNumNumBool op = binaryFunc func
  where
    func :: Datum -> Datum -> Result Datum
    func (Real a) (Real b) = return $ Boolean $ op a b
    func (Integer a) (Integer b) = return $ Boolean $ op a b
    func (Integer a) b = func (Real $ fromIntegral a) b
    func a (Integer b) = func a (Real $ fromIntegral b)
    func x y = throwError $ TypeMismatch "number" (Pair x y)

data AsDivResult = AsInt | AsReal | AsReminder

numDivision :: AsDivResult -> Environment -> [Datum] -> Result Datum
numDivision d = binaryFunc $ func d
  where
    func :: AsDivResult -> Datum -> Datum -> Result Datum
    func AsInt (Integer a) (Integer b) = return $ Integer (a `div` b)
    func AsInt (Real a) (Real b) = return $ Integer $ floor (a / b)
    func AsReal (Integer a) (Integer b) = return $ Real $ (fromIntegral a) / (fromIntegral b)
    func AsReal (Real a) (Real b) = return $ Real (a / b)
    func AsReminder (Integer a) (Integer b) = return $ Integer (a `mod` b)
    func d (Integer a) b = func d (Real $ fromIntegral a) b
    func d a (Integer b) = func d a (Real $ fromIntegral b)
    func _ x y = throwError $ TypeMismatch "numbers" (Pair x y)

----------------------
-- String functions --
----------------------

opStrStrStr :: (String -> String -> String) -> Environment -> [Datum] -> Result Datum
opStrStrStr op = binaryFunc func
  where
    func :: Datum -> Datum -> Result Datum
    func (String a) (String b) = return $ String $ op a b
    func x y = throwError $ TypeMismatch "string" (Pair x y)

opStrStrBool :: (String -> String -> Bool) -> Environment -> [Datum] -> Result Datum
opStrStrBool op = binaryFunc func
  where
    func :: Datum -> Datum -> Result Datum
    func (String a) (String b) = return $ Boolean $ op a b
    func x y = throwError $ TypeMismatch "string" (Pair x y)

strSubstring :: Environment -> [Datum] -> Result Datum
strSubstring = ternaryFunc func
  where
    func :: Datum -> Datum -> Datum -> Result Datum
    func (String s) (Integer beg) (Integer end) = return $ String $ take (end - beg) (drop beg s)
    func x y z = throwError $ TypeMismatch "string, int, int" (pair [x, y, z])

strStringNewline :: Environment -> [Datum] -> Result Datum
strStringNewline = unaryFunc func
  where
    func :: Datum -> Result Datum
    func (String s) = return $ String (s ++ "\n")
    func x = throwError $ TypeMismatch "string" x

hashElems :: (H.HashMap String Datum -> [Datum]) -> Environment -> [Datum] -> Result Datum
hashElems hfunc _ [Hash h] = return . pair $ hfunc h
hashElems _ _ _ = throwError $ Default "Invalid argument for hash-keys"

-------------------------
-- Container functions --
-------------------------

data Container = StrCtner | VecCtner | HashCtner deriving (Eq)

containerAt :: Container -> Environment -> [Datum] -> Result Datum
containerAt ct = binaryFunc (func ct)
  where
    func :: Container -> Datum -> Datum -> Result Datum
    func StrCtner (Integer i) (String s) = return $ Character $ s !! i
    func VecCtner (Integer i) (Vector v) = return $ v V.! i
    func HashCtner (String k) (Hash htb) = return $ maybe Void id (H.lookup k htb)
    func _ x y = throwError $ TypeMismatch "index, container" (Pair x y)

containerLen :: Container -> Environment -> [Datum] -> Result Datum
containerLen ct = unaryFunc (func ct)
  where
    func :: Container -> Datum -> Result Datum
    func StrCtner (String s) = return $ Integer $ length s
    func VecCtner (Vector v) = return $ Integer $ V.length v
    func HashCtner (Hash ht) = return $ Integer $ H.size ht
    func _ other = throwError $ TypeMismatch "container" other

containerSet :: Container -> Environment -> [Datum] -> Result Datum
containerSet StrCtner env [Symbol name, Integer i, Character c] =
  eval env (Symbol name) >>= \case
    (String s) -> mutateEnv env name (String $ changeChr s)
    other -> throwError $ TypeMismatch "string" other
  where
    changeChr s = (fst $ splitAt i s) ++ c : (tail . snd $ splitAt i s)
containerSet VecCtner env [Symbol name, Integer i, value] =
  eval env (Symbol name) >>= \case
    (Vector v) -> mutateEnv env name (Vector $ v V.// [(i, value)])
    other -> throwError $ TypeMismatch "vectora" other
containerSet HashCtner env [Symbol name, String k, value] =
  eval env (Symbol name) >>= \case
    (Hash htb) -> mutateEnv env name (Hash $ H.insert k value htb)
    other -> throwError $ TypeMismatch "string" other
containerSet _ _ x = throwError $ TypeMismatch "container, index, value" (pair x)

containerFill :: Container -> Environment -> [Datum] -> Result Datum
containerFill StrCtner env [Symbol name, Character c] =
  eval env (Symbol name) >>= \case
    (String s) -> mutateEnv env name (String $ replicate (length s) c)
    other -> throwError $ TypeMismatch "string" other
containerFill VecCtner env [Symbol name, value] =
  eval env (Symbol name) >>= \case
    (Vector v) -> mutateEnv env name (Vector $ V.map (const value) v)
containerFill _ _ x = throwError $ TypeMismatch "container, value" (pair x)

--------------------
-- List functions --
--------------------

car :: Environment -> [Datum] -> Result Datum
car = unaryFunc func
  where
    func :: Datum -> Result Datum
    func (Pair a _) = return a
    func x = throwError $ TypeMismatch "pair" x

cdr :: Environment -> [Datum] -> Result Datum
cdr = unaryFunc func
  where
    func :: Datum -> Result Datum
    func (Pair _ b) = return b
    func x = throwError $ TypeMismatch "pair" x

cons :: Environment -> [Datum] -> Result Datum
cons = binaryFunc (\x y -> return $ Pair x y)

isNil :: Environment -> [Datum] -> Result Datum
isNil _ [(Symbol "()")] = return $ Boolean True
isNil _ _ = return $ Boolean False

------------------
-- IO functions --
------------------

makePort :: IOMode -> Environment -> [Datum] -> Result Datum
makePort mode = unaryFunc func
  where
    func :: Datum -> Result Datum
    func (String fname) = liftIO $ liftM (Port) $ openFile fname mode
    func x = throwError $ TypeMismatch "string" x

closePort :: Environment -> [Datum] -> Result Datum
closePort _ [Port port] = liftIO $ hClose port >> return (Boolean True)
closePort _ [] = return $ Boolean False

readProc :: Environment -> [Datum] -> Result Datum
readProc _ [Port port] = liftIO $ hGetLine port >>= return . String
readProc e [] = readProc e [Port stdin]
readProc _ [x] = throwError $ TypeMismatch "port" x
readProc _ x = throwError $ NumArgs 1 x

writeProc :: Environment -> [Datum] -> Result Datum
writeProc _ [String str, Port port] = liftIO $ hPutStrLn port str >> return Void
writeProc _ [obj, Port port] = liftIO $ hPrint port obj >> return Void
writeProc e [obj] = writeProc e [obj, Port stdout]
writeProc _ x = throwError $ NumArgs 1 x

readContents :: Environment -> [Datum] -> Result Datum
readContents _ [String fname] = liftIO $ readFile fname >>= return . String
readContents _ [Port port] = liftIO $ hGetContents port >>= return . String
readContents _ [arg] = throwError $ TypeMismatch "string" arg
readContents _ args = throwError $ NumArgs 1 args

---------------------------
-- Polymorphic functions --
---------------------------

isEq :: Environment -> [Datum] -> Result Datum
isEq = binaryFunc func
  where
    func :: Datum -> Datum -> Result Datum
    func (Symbol s1) (Symbol s2) = return $ Boolean $ s1 == s2
    func (Boolean b1) (Boolean b2) = return $ Boolean $ b1 == b2
    func (Character c1) (Character c2) = return $ Boolean $ c1 == c2
    func (Integer i1) (Integer i2) = return $ Boolean $ i1 == i2
    func (Real r1) (Real r2) = return $ Boolean $ r1 == r2
    func (String s1) (String s2) = return $ Boolean $ s1 == s2
    func p1@(Pair _ _) p2@(Pair _ _) =
      func (Vector $ V.fromList $ unpair p1) (Vector $ V.fromList $ unpair p2)
    func (Vector _) (Vector _) = return $ Boolean False

isSame :: Environment -> [Datum] -> Result Datum
isSame env other = isEq env other

isSym :: Environment -> [Datum] -> Result Datum
isSym = $(isDatumType [p|(Symbol _)|])

isBool :: Environment -> [Datum] -> Result Datum
isBool = $(isDatumType [p|(Boolean _)|])

isChr :: Environment -> [Datum] -> Result Datum
isChr = $(isDatumType [p|(Character _)|])

isInt :: Environment -> [Datum] -> Result Datum
isInt = $(isDatumType [p|(Integer _)|])

isReal :: Environment -> [Datum] -> Result Datum
isReal = $(isDatumType [p|(Real _)|])

isStr :: Environment -> [Datum] -> Result Datum
isStr = $(isDatumType [p|(String _)|])

isVec :: Environment -> [Datum] -> Result Datum
isVec = $(isDatumType [p|(Vector _)|])

isHash :: Environment -> [Datum] -> Result Datum
isHash = $(isDatumType [p|(Hash _)|])

isPort :: Environment -> [Datum] -> Result Datum
isPort = $(isDatumType [p|(Port _)|])

isList :: Environment -> [Datum] -> Result Datum
isList = $(isDatumType [p|(Pair _ _)|])

isEnv :: Environment -> [Datum] -> Result Datum
isEnv = $(isDatumType [p|(Environment _)|])

isProc :: Environment -> [Datum] -> Result Datum
isProc = $(isDatumType [p|(Procedure {})|])

symToStr :: Datum -> Result Datum
symToStr (Symbol s) = return $ String s
symToStr x = throwError $ TypeMismatch "symbol" x

boolToStr :: Datum -> Result Datum
boolToStr (Boolean True) = return $ String "#t"
boolToStr (Boolean False) = return $ String "#f"
boolToStr x = throwError $ TypeMismatch "" x

chrToStr :: Datum -> Result Datum
chrToStr (Character c) = return $ String [c]
chrToStr x = throwError $ TypeMismatch "" x

chrToInt :: Datum -> Result Datum
chrToInt (Character c) = return $ Integer $ ord c
chrToInt x = throwError $ TypeMismatch "" x

intToChr :: Datum -> Result Datum
intToChr (Integer i) = return $ Character $ chr i
intToChr x = throwError $ TypeMismatch "" x

numToStr :: Datum -> Result Datum
numToStr (Integer i) = return $ String $ show i
numToStr (Real r) = return $ String $ show r
numToStr x = throwError $ TypeMismatch "" x

strToSym :: Datum -> Result Datum
strToSym (String s) = return $ Symbol s
strToSym x = throwError $ TypeMismatch "" x

strToInt :: Datum -> Result Datum
strToInt (String s) = return $ Integer $ read s
strToInt x = throwError $ TypeMismatch "" x

strToReal :: Datum -> Result Datum
strToReal (String s) = return $ Real $ read s
strToReal x = throwError $ TypeMismatch "" x

strToVec :: Datum -> Result Datum
strToVec (String s) = return $ Vector $ V.fromList $ map (Character) s
strToVec x = throwError $ TypeMismatch "" x

vecToStr :: Datum -> Result Datum
vecToStr (Vector v) =
  -- ! Invalid inputs are silently ignored
  let chars = V.mapMaybe (\x -> case x of Character c -> Just c; _ -> Nothing) v
   in return $ String $ V.toList $ chars
vecToStr x = throwError $ TypeMismatch "" x

vecToList :: Datum -> Result Datum
vecToList (Vector v) = return $ pair $ V.toList v
vecToList x = throwError $ TypeMismatch "" x

hashToVec :: Datum -> Result Datum
hashToVec (Hash htb) =
  let keys = map (String) $ H.keys htb
      pairs = zip keys (H.elems htb)
      pairVals = map (\(x, y) -> Pair x y) pairs
   in return $ Vector $ V.fromList pairVals
hashToVec x = throwError $ TypeMismatch "" x

hashToEnv :: Datum -> Result Datum
hashToEnv (Hash h) = do
  elems <- liftIO $ mapM newIORef (H.elems h)
  env <- liftIO $ newIORef $ zip (H.keys h) elems
  return $ Environment env
hashToEnv x = throwError $ TypeMismatch "" x
