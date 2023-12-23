module Logic where

import Data.Char
import Control.Applicative (Alternative, empty, (<|>), many)
import System.Environment
import Data.Text.Array (new, run)
import Data.IntMap.Merge.Lazy (merge)
import GHC.IO
import Parser
import Types
import System.Process
import System.Exit
import GHC.IO.Handle
import Text.Read (Lexeme(Ident))


getStr :: Int -> [(String, Atom)] -> Atom -> String
getStr 0 v (StrVal s) = "\"" ++ s ++ "\""
getStr 1 v (StrVal s) = s
getStr _ v (FltVal f) = show f
getStr _ v (IntVal i) = show i
getStr _ v (BoolVal True) = "true"
getStr _ v (BoolVal False) = "false"
getStr _ v Null = "null"
getStr _ v (ListVal l) = getStrList v l

getStrList :: [(String, Atom)] -> [Atom] -> String
getStrList v [] = "[]"
getStrList v (x:xs) = "[" ++ content ++ "]"
  where
    content = foldl (\acc x -> acc ++ "," ++ getStr 0 v x) (getStr 0 v x) xs

--evaluate an Operation
eval:: [(String, [String], Structure)] -> [(String, Atom)] -> Op -> IO Atom
eval f v (Value (Identifier s)) =
  case lookup s v of
    Just a -> return a
    Nothing -> error $ "Variable not found: " ++ s
eval f v (Value a) = return a
eval f v (Add e1 e2) = evalAdd f v e1 e2
eval f v (Sub e1 e2) = evalSub f v e1 e2
eval f v (Mul e1 e2) = evalMul f v e1 e2
eval f v (Pow e1 e2) = evalPow f v e1 e2
eval f v (Div e1 e2) = evalDiv f v e1 e2
eval f v (Mod e1 e2) = evalMod f v e1 e2
eval f v (Comp e1 e2) = evalComp f v e1 e2
eval f v (NComp e1 e2) = do
  (BoolVal res) <- evalComp f v e1 e2
  return $ BoolVal (not res)
eval f v (Bigger e1 e2) = evalBigger f v e1 e2
eval f v (Smaller e1 e2) = evalBigger f v e2 e1
eval f v (BiggerEq e1 e2) = do
  (BoolVal res) <- evalBigger f v e2 e1
  return $ BoolVal (not res)
eval f v (SmallerEq e1 e2) = do
  (BoolVal res) <- evalBigger f v e1 e2
  return $ BoolVal (not res)
eval f v (And e1 e2) = evalAnd f v e1 e2
eval f v (Or e1 e2) = evalOr f v e1 e2
eval f v (Not e1) = evalNot f v e1
eval f v (AccessList e1 e2) = evalAccessList f v e1 e2
eval f v (Length x) = evalLength f v x
eval f v (CreateList ops) = do
  a <- mapM (eval f v) ops
  return $ ListVal a
eval f v (Bracket e1) = eval f v e1
eval f v (FuncCall name args) = evalFuncCall f v name args
eval f v (ListComprehension op ((name,list):xs) cond) = evalListComp f v op name list xs cond
eval f v (Ternary cond e1 e2) = do
  (BoolVal res) <- eval f v cond
  if res then eval f v e1 else eval f v e2

evalAdd f v e1 e2 = do
  a <- eval f v e1
  b <- eval f v e2
  case (a,b) of
    (IntVal x1, IntVal x2) -> return $ IntVal (x1+x2)
    (FltVal x1, IntVal x2) -> return $ FltVal (x1+fromIntegral x2)
    (IntVal x1, FltVal x2) -> return $ FltVal (fromIntegral x1+x2)
    (FltVal x1, FltVal x2) -> return $ FltVal (x1+x2)
    (ListVal x1, ListVal x2) -> return $ ListVal (x1 ++ x2)
    (StrVal x1, StrVal x2) -> return $ StrVal (x1 ++ x2)
    (StrVal x1, x2) -> return $ StrVal (x1 ++ getStr 0 v x2)
    (x1, StrVal x2) -> return $ StrVal (getStr 0 v x1 ++ x2)
    _ -> error "Type error while evaluating '+'"

evalSub f v e1 e2 = do
  a <- eval f v e1
  b <- eval f v e2
  case (a,b) of
    (IntVal x1, IntVal x2) -> return $ IntVal (x1-x2)
    (FltVal x1, IntVal x2) -> return $ FltVal (x1-fromIntegral x2)
    (IntVal x1, FltVal x2) -> return $ FltVal (fromIntegral x1-x2)
    (FltVal x1, FltVal x2) -> return $ FltVal (x1-x2)
    _ -> error "Type error while evaluating '-'"

evalMul f v e1 e2 = do
  a <- eval f v e1
  b <- eval f v e2
  case (a,b) of
    (IntVal x1, IntVal x2) -> return $ IntVal (x1*x2)
    (FltVal x1, IntVal x2) -> return $ FltVal (x1*fromIntegral x2)
    (IntVal x1, FltVal x2) -> return $ FltVal (fromIntegral x1*x2)
    (FltVal x1, FltVal x2) -> return $ FltVal (x1*x2)
    _ -> error "Type error while evaluating '*'"

evalDiv f v e1 e2 = do
  a <- eval f v e1
  b <- eval f v e2
  case (a,b) of
    (IntVal x1, IntVal x2) -> return $ IntVal (x1 `div` x2)
    (FltVal x1, IntVal x2) -> return $ FltVal (x1/fromIntegral x2)
    (IntVal x1, FltVal x2) -> return $ FltVal (fromIntegral x1/x2)
    (FltVal x1, FltVal x2) -> return $ FltVal (x1/x2)
    _ -> error "Type error while evaluating '/'"

evalPow f v e1 e2 = do
  a <- eval f v e1
  b <- eval f v e2
  case (a,b) of
    (IntVal x1, IntVal x2) -> return $ IntVal (x1^x2)
    (FltVal x1, IntVal x2) -> return $ FltVal (x1^^x2)
    (IntVal x1, FltVal x2) -> return $ FltVal (fromIntegral x1**x2)
    (FltVal x1, FltVal x2) -> return $ FltVal (x1**x2)
    _ -> error "Type error while evaluating '**'"

evalMod f v e1 e2 = do
  a <- (eval f v e1)
  b <- (eval f v e2)
  case (a,b) of
     (IntVal x1, IntVal x2) -> return $ IntVal (x1 `mod` x2)
     _ -> error "Type error while evaluating '%'"

evalComp f v e1 e2 = do
  a <- (eval f v e1)
  b <- (eval f v e2)
  case (a,b) of
    (IntVal x1, IntVal x2) -> return $ BoolVal (x1 == x2)
    (FltVal x1, IntVal x2) -> return $ BoolVal (x1 == fromIntegral x2)
    (IntVal x1, FltVal x2) -> return $ BoolVal (fromIntegral x1 == x2)
    (FltVal x1, FltVal x2) -> return $ BoolVal (x1 == x2)
    (StrVal x1, StrVal x2) -> return $ BoolVal (x1 == x2)
    (BoolVal x1, BoolVal x2) -> return $ BoolVal (x1 == x2)
    (ListVal x1, ListVal x2) -> return $ BoolVal (x1 == x2)
    (Null, Null) -> return $ BoolVal True
    _ -> return $ BoolVal False

evalBigger f v e1 e2 = do
  a <- (eval f v e1)
  b <- (eval f v e2)
  case (a,b) of
    (IntVal x1, IntVal x2) -> return $ BoolVal (x1 > x2)
    (FltVal x1, IntVal x2) -> return $ BoolVal (x1 > fromIntegral x2)
    (IntVal x1, FltVal x2) -> return $ BoolVal (fromIntegral x1 > x2)
    (FltVal x1, FltVal x2) -> return $ BoolVal (x1 > x2)
    _ -> error "Type error while evaluating '>'"

evalAnd f v e1 e2 = do
  a <- (eval f v e1)
  b <- (eval f v e2)
  case (a,b) of
    (BoolVal x1, BoolVal x2) -> return $ BoolVal (x1 && x2)
    _ -> error "Type error while evaluating '&&'"

evalOr f v e1 e2 = do
  a <- (eval f v e1)
  b <- (eval f v e2)
  case (a,b) of
    (BoolVal x1, BoolVal x2) -> return $ BoolVal (x1 || x2)
    _ -> error "Type error while evaluating '||'"

evalNot f v e1 =  do
  a <- (eval f v e1)
  case a of
    (BoolVal x1) -> return $ BoolVal (not x1)
    _ -> error "Type error while evaluating '!'"

evalAccessList f v e1 e2 = do
  a <- (eval f v e1)
  b <- (eval f v e2)
  case (a,b) of
    (ListVal l, IntVal i) -> return $ l !! i
    (StrVal s, IntVal i) -> return $ StrVal [s !! i]
    _ -> error "Type error while evaluating '!'"

evalLength f v x = do
  a <- (eval f v x)
  case a of
    (ListVal l) -> return $ IntVal (length l)
    (StrVal s) -> return $ IntVal (length s)

evalFuncCall f v "os" args = do
  if length args == 1
    then do
      (StrVal userIn) <- eval f v (head args)
      (command, args) <- case words userIn of
        (x:xs) -> return (x, xs)
        [] -> error "Invalid number of arguments for os"
      (inp, out, err, handle) <- runInteractiveCommand (unwords (command:args))
      hClose inp
      result <- hGetContents out
      return $ StrVal result
    else error "Invalid number of arguments for os"
evalFuncCall f v "input" args = do
  StrVal <$> getLine
evalFuncCall f v "error" args = do
  (StrVal msg) <- eval f v (head args)
  putStrLn ("Error: " ++ msg)
  exitSuccess
evalFuncCall f v "int" args = cast f v "int" (head args)
evalFuncCall f v "float" args = cast f v "float" (head args)
evalFuncCall f v "str" args = cast f v "str" (head args)
evalFuncCall f v "bool" args = cast f v "bool" (head args)
evalFuncCall f v "types" args = do
  val <- eval f v (head args)
  case val of
    (IntVal _) -> return $ StrVal "int"
    (FltVal _) -> return $ StrVal "float"
    (StrVal _) -> return $ StrVal "str"
    (BoolVal _) -> return $ StrVal "bool"
    (ListVal _) -> return $ StrVal "list"
    Null -> return $ StrVal "null"
evalFuncCall f v name args = do
  params <- mapM (eval f v) args
  case find f name of
    Just (paramNames, body) -> do
      let addP = zip paramNames params
      (newV,_) <- executeC f addP body
      case lookup "return" newV of
        Just a -> return a
        Nothing -> return Null
    Nothing -> error $ "Function not found: " ++ name

evalListComp f v op name list xs cond = do
  ListVal evald <- eval f v list
  res <- listC v xs (name, evald)
  return $ ListVal res
  where
    listC vars _ (name, []) = return []
    listC vars [] (name, (x:xs)) = do
      cond <- eval f ((name, x):vars) cond
      if cond == BoolVal True
        then do
          rest <- listC vars [] (name, xs)
          curr <- eval f ((name, x):vars) op
          return $ curr:rest
        else listC vars [] (name, xs)
    listC vars ((name, list):xs) (currName, (v:vs)) = do
      ListVal next <- eval f vars list
      currentRun <- listC ((currName, v):vars) xs (name, next)
      rest <- listC vars ((name, list):xs) (currName, vs)
      return $ currentRun ++ rest

cast :: [(String, [String], Structure)] -> [(String, Atom)] -> String -> Op -> IO Atom
cast f v typ op = do
  val <- eval f v op
  case typ of
    "int" -> case val of
      (IntVal i) -> return $ IntVal i
      (FltVal f) -> return $ IntVal (round f)
      (StrVal s) -> return $ IntVal (read s :: Int)
      (BoolVal True) -> return $ IntVal 1
      (BoolVal False) -> return $ IntVal 0
      _ -> error "Type error while casting to int"
    "float" -> case val of
      (IntVal i) -> return $ FltVal (fromIntegral i)
      (FltVal f) -> return $ FltVal f
      (StrVal s) -> return $ FltVal (read s :: Double)
      (BoolVal True) -> return $ FltVal 1.0
      (BoolVal False) -> return $ FltVal 0.0
      _ -> error "Type error while casting to float"
    "str" -> case val of
      (IntVal i) -> return $ StrVal (show i)
      (FltVal f) -> return $ StrVal (show f)
      (StrVal s) -> return $ StrVal s
      (BoolVal True) -> return $ StrVal "true"
      (BoolVal False) -> return $ StrVal "false"
      _ -> error "Type error while casting to str"
    "bool" -> case val of
      (IntVal i) -> return $ BoolVal (i /= 0)
      (FltVal f) -> return $ BoolVal (f /= 0.0)
      (StrVal s) -> return $ BoolVal (s /= "")
      (BoolVal b) -> return $ BoolVal b
      Null -> return $ BoolVal False
      _ -> error "Type error while casting to bool"
    _ -> error "Invalid cast"


find :: [(String, [String], Structure)] -> String -> Maybe ([String], Structure)
find [] name = Nothing
find ((fname, params, body):xs) name = if fname == name then Just (params, body) else find xs name

assign:: [(String, [String], Structure)] ->  [(String, Atom)] -> Structure -> IO [(String, Atom)]
assign f v (Assign name val) = do
  evaled <- eval f v val
  case lookup name v of
    Just a -> return $ (name, evaled):(filter (\x -> fst x /= name) v)
    Nothing -> return $ (name, evaled):v

mergev:: [(String, Atom)] -> [(String, Atom)] -> [(String, Atom)]
mergev [] new = case (lookup "break" new, lookup "return" new) of
  (Just b, Just r) -> [("break", b), ("return", r)]
  (Just b, Nothing) -> [("break", b)]
  (Nothing, Just r) -> [("return", r)]
  _ -> []
mergev (x:xs) new = case lookup (fst x) new of
  Just a -> (fst x, a):(mergev xs new)
  Nothing -> if fst x == "break" then x:(mergev xs new) else mergev xs new

executeC :: [(String, [String], Structure)] -> [(String, Atom)] -> Structure -> IO ([(String, Atom)], [(String, [String], Structure)])
executeC f v l = if lookup "return" v /= Nothing || lookup "break" v /= Nothing then return (v,f) else execute f v l

execute:: [(String, [String ], Structure)] -> [(String, Atom)] -> Structure -> IO ([(String, Atom)], [(String, [String ], Structure)])
execute f v (Lines []) = return (v,f)
execute f v (Lines ((EmptyLine):xs)) = execute f v (Lines xs)
execute f v (Lines ((IfElse condition ifBody elseBody):xs)) = do
  (BoolVal cond) <- eval f v condition
  (newv,_) <- if cond then executeC f v ifBody else executeC f v elseBody
  executeC f (mergev v newv) (Lines xs)
execute f v (Lines ((While condition body):xs)) = do
  (BoolVal cond) <- eval f v condition
  if cond
    then do
      (newv,_) <- execute f v body
      let mergedv = mergev v newv
      case lookup "break" mergedv of
        Just _ -> executeC f (filter (\(var, _) -> var /= "break") mergedv) (Lines xs)
        Nothing -> executeC f mergedv (Lines ((While condition body):xs))
    else executeC f v (Lines xs)
execute f v (Lines ((Break:xs))) = do
  return (("break", BoolVal True) : v,f)
execute f v (Lines ((Print e:xs))) = do
  evald <- eval f v e
  putStrLn (getStr 1 v (evald))
  execute f v (Lines xs)
execute f v (Lines ((Assign var (CreateList ops):xs))) = do
  value <- eval f v (CreateList ops)
  execute f ((var, value) : v) (Lines xs)
execute f v (Lines ((Assign var op:xs))) = do
  value <- eval f v op
  execute f ((var, value) : v) (Lines xs)
execute f v (Lines ((AssignFromFile name val:xs))) = do
  (StrVal path) <- eval f v val
  input <- readFile path
  vars <- assign f v (Assign name (Value (StrVal input)))
  execute f vars (Lines xs)
execute f v (Lines ((OverwriteFile val file:xs))) = do
  (StrVal path) <- eval f v file
  content <- eval f v val
  writeFile path (getStr 1 v content)
  execute f v (Lines xs)
execute f v (Lines ((AppendFile val file:xs))) = do
  (StrVal path) <- eval f v file
  content <- eval f v val
  appendFile path (getStr 1 v content)
  execute f v (Lines xs)
execute f v (Lines ((Function name params body:xs))) = do
  execute ((name, params, body):f) v (Lines xs)
execute f v (Lines ((ReturnVal val:xs))) = do
  value <- eval f v val
  return (("return", value):v, f)
execute f v (Lines ((Return:xs))) = return (("return", StrVal ""):v, f)
execute f v (Lines ((Import path:xs))) = do
  let newPath = if path == "std" then "/usr/local/lib/scriptLang/std.sc" else path
  input <- readFile newPath
  case runParser (indentedLinesP 0) (input ++ "\n") of
    Just ("", lines) -> do
      (vars, funcs) <- execute f v lines
      execute (funcs ++ f) (vars ++ v) (Lines xs)
    Just (rest, _) -> error $ "Unparsed (import "++path++"): \n" ++ rest
    Nothing -> error $ "Invalid input (import " ++ path ++ ")"
execute f v (Lines ((FCall name args:xs))) = do
  _ <- eval f v (FuncCall name args)
  execute f v (Lines xs)
execute f v (Lines ((For name initial condition increment body):xs)) = do
  val <- eval f v initial
  let (Lines oldBody) = body
  let newBody = Lines (oldBody ++ [Assign name increment])
  (whileV, newF) <- execute f ((name, val):v) (Lines ([While condition newBody]))
  let newV = filter (\(var, _) -> var /= name) whileV
  executeC newF newV (Lines xs)
execute f v (Lines ((ForEach name clist body):xs)) = do
  (ListVal list) <- eval f v clist
  (newv,_) <- runForEach f v name list body
  let mergedv = mergev v newv
  executeC f mergedv (Lines xs)

runForEach:: [(String, [String], Structure)] -> [(String, Atom)] -> String -> [Atom] -> Structure -> IO ([(String, Atom)], [(String, [String], Structure)])
runForEach f v name [] body = return (filter (\(var, _) -> var /= name) v, f)
runForEach f v name (x:xs) body = do
  (newv,_) <- execute f ((name, x):v) body
  let mergedv = mergev v newv
  case lookup "break" mergedv of
    Just _ -> return (("break", BoolVal True) : v,f)
    Nothing -> runForEach f mergedv name xs body