module Logic where

import Data.Char
import Control.Applicative (Alternative, empty, (<|>), many)
import System.Environment

data Atom
  = IntVal Int
  | FltVal Double
  | StrVal String
  | BoolVal Bool
  | ListVal [Atom]
  | VarName String
  | FileName String
  deriving (Show, Eq)

data Op
  = Add Op Op
  | Sub Op Op
  | Mul Op Op
  | Div Op Op
  | Mod Op Op
  | Pow Op Op
  | Comp Op Op
  | NComp Op Op
  | Bigger Op Op
  | Smaller Op Op
  | BiggerEq Op Op
  | SmallerEq Op Op
  | And Op Op
  | Or Op Op
  | Not Op
  | CreateList [Op]
  | AccessList Op Op
  | Length Op
  | Value Atom
  | Bracket Op
  deriving (Show, Eq)

data Structure
  = Lines [Structure]
  | Assign String Op
  | AssignFromFile String Op
  | Print Op
  | OverwriteFile Op Op
  | AppendFile Op Op
  | IfElse Op Structure Structure
  | While Op Structure
  | EmptyLine
  deriving Show

getStr :: Int -> [(String, Atom)] -> Atom -> String
getStr 0 vars (StrVal s) = "\"" ++ s ++ "\""
getStr 1 vars (StrVal s) = s
getStr _ vars (FltVal f) = show f
getStr _ vars (IntVal i) = show i
getStr _ vars (BoolVal True) = "true"
getStr _ vars (BoolVal False) = "false"
getStr _ vars (ListVal l) = getStrList vars l

getStrList :: [(String, Atom)] -> [Atom] -> String
getStrList vars [] = "[]"
getStrList vars (x:xs) = "[" ++ content ++ "]"
  where
    content = foldl (\acc x -> acc ++ "," ++ getStr 0 vars x) (getStr 0 vars x) xs


--evaluate an Operation
eval:: [(String, Atom)] -> Op -> Atom
eval vars (Value (VarName s)) = 
  case lookup s vars of
    Just a -> a
    Nothing -> error $ "Variable not found: " ++ s
eval vars (Value a) = a
eval vars (Add e1 e2) = 
  case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> IntVal (x1+x2)
    (FltVal x1, IntVal x2) -> FltVal (x1+fromIntegral x2)
    (IntVal x1, FltVal x2) -> FltVal (fromIntegral x1+x2)
    (FltVal x1, FltVal x2) -> FltVal (x1+x2)
    (ListVal x1, ListVal x2) -> ListVal (x1 ++ x2)
    (StrVal x1, StrVal x2) -> StrVal (x1 ++ x2)
    (StrVal x1, x2) -> StrVal (x1 ++ getStr 0 vars x2)
    (x1, StrVal x2) -> StrVal (getStr 0 vars x1 ++ x2)
    _ -> error "Type error while evaluating '+'"
eval vars (Sub e1 e2) =
  case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> IntVal (x1-x2)
    (FltVal x1, IntVal x2) -> FltVal (x1-fromIntegral x2)
    (IntVal x1, FltVal x2) -> FltVal (fromIntegral x1-x2)
    (FltVal x1, FltVal x2) -> FltVal (x1-x2)
    _ -> error "Type error while evaluating '-'"
eval vars (Mul e1 e2) =
 case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> IntVal (x1*x2)
    (FltVal x1, IntVal x2) -> FltVal (x1*fromIntegral x2)
    (IntVal x1, FltVal x2) -> FltVal (fromIntegral x1*x2)
    (FltVal x1, FltVal x2) -> FltVal (x1*x2)
    _ -> error "Type error while evaluating '*'"
eval vars (Pow e1 e2) = 
  case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> IntVal (x1^x2)
    (FltVal x1, IntVal x2) -> FltVal (x1^^x2)
    (IntVal x1, FltVal x2) -> FltVal (fromIntegral x1**x2)
    (FltVal x1, FltVal x2) -> FltVal (x1**x2)
    _ -> error "Type error while evaluating '**'"
eval vars (Div e1 e2) =
 case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> IntVal (x1 `div` x2)
    (FltVal x1, IntVal x2) -> FltVal (x1/fromIntegral x2)
    (IntVal x1, FltVal x2) -> FltVal (fromIntegral x1/x2)
    (FltVal x1, FltVal x2) -> FltVal (x1/x2)
    _ -> error "Type error while evaluating '/'"
eval vars (Mod e1 e2) =
 case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> IntVal (x1 `mod` x2)
    _ -> error "Type error while evaluating '%'"
eval vars (Comp e1 e2) =
  case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> BoolVal (x1 == x2)
    (FltVal x1, IntVal x2) -> BoolVal (x1 == fromIntegral x2)
    (IntVal x1, FltVal x2) -> BoolVal (fromIntegral x1 == x2)
    (FltVal x1, FltVal x2) -> BoolVal (x1 == x2)
    (StrVal x1, StrVal x2) -> BoolVal (x1 == x2)
    (BoolVal x1, BoolVal x2) -> BoolVal (x1 == x2)
    (ListVal x1, ListVal x2) -> BoolVal (x1 == x2)
    _ -> error "Type error while evaluating '=='"
eval vars (NComp e1 e2) = let (BoolVal x) = eval vars (Comp e1 e2) in BoolVal (not x)
eval vars (Bigger e1 e2) = 
  case (eval vars e1, eval vars e2) of
    (IntVal x1, IntVal x2) -> BoolVal (x1 > x2)
    (FltVal x1, IntVal x2) -> BoolVal (x1 > fromIntegral x2)
    (IntVal x1, FltVal x2) -> BoolVal (fromIntegral x1 > x2)
    (FltVal x1, FltVal x2) -> BoolVal (x1 > x2)
    _ -> error "Type error while evaluating '>'"
eval vars (Smaller e1 e2) = eval vars (Bigger e2 e1)
eval vars (BiggerEq e1 e2) = let (BoolVal x) = eval vars (Smaller e1 e2) in BoolVal (not x)
eval vars (SmallerEq e1 e2) = let (BoolVal x) = eval vars (Bigger e1 e2) in BoolVal (not x)
eval vars (And e1 e2) =
  case (eval vars e1, eval vars e2) of
    (BoolVal x1, BoolVal x2) -> BoolVal (x1 && x2)
    _ -> error "Type error while evaluating '&&'"
eval vars (Or e1 e2) =
  case (eval vars e1, eval vars e2) of
    (BoolVal x1, BoolVal x2) -> BoolVal (x1 || x2)
    _ -> error "Type error while evaluating '||'"
eval vars (Not e1) =
  case (eval vars e1) of
    (BoolVal x1) -> BoolVal (not x1)
    _ -> error "Type error while evaluating '!'"
eval vars (AccessList e1 e2) = 
  case (eval vars e1, eval vars e2) of
    (ListVal list, IntVal index) -> list !! index
    (StrVal str, IntVal index) -> StrVal [str !! index]
    _ -> error "Type error while evaluating '!'"
eval vars (Length x) = 
  case (eval vars x) of
    (ListVal l) -> IntVal (length l)
    (StrVal s) -> IntVal (length s)
eval vars (CreateList ops) = ListVal (map (eval vars) ops)
eval vars (Bracket e1) = eval vars e1
 

assign:: [(String, Atom)] -> Structure -> [(String, Atom)]
assign vars (Assign name val) = 
  case lookup name vars of
    Just a -> (name, eval vars val):(filter (\x -> fst x /= name) vars)
    Nothing -> (name, eval vars val):vars

mergeVars:: [(String, Atom)] -> [(String, Atom)] -> [(String, Atom)]
mergeVars [] _ = []
mergeVars (x:xs) new = case lookup (fst x) new of
  Just a -> (fst x, a):(mergeVars xs new)
  Nothing -> x:(mergeVars xs new)

execute:: [(String, Atom)] -> Structure -> IO ([(String, Atom)])
execute vars (Lines []) = return (vars)
execute vars (Lines ((EmptyLine):xs)) = execute vars (Lines xs)
execute vars (Lines ((IfElse condition ifBody elseBody):xs)) = do
  let (BoolVal cond) = eval vars condition
  newVars <- if cond then execute vars ifBody else execute vars elseBody
  execute (mergeVars vars newVars) (Lines xs)
execute vars (Lines ((While condition body):xs)) = do
  let (BoolVal cond) = eval vars condition
  if cond 
    then do
      newVars <- execute vars body
      execute (mergeVars vars newVars) (Lines ((While condition body):xs))
    else execute vars (Lines xs)
execute vars (Lines ((Print e:xs))) = do
  putStrLn (getStr 0 vars (eval vars e))
  execute vars (Lines xs)
execute vars (Lines ((Assign var (CreateList ops):xs))) = do
  let evaluatedList = ListVal (map (eval vars) ops)
  execute ((var, evaluatedList) : vars) (Lines xs)
execute vars (Lines ((Assign var op:xs))) = do
  let value = eval vars op
  execute ((var, value) : vars) (Lines xs)
execute vars (Lines ((AssignFromFile name val:xs))) = do
  let (StrVal path) = eval vars val
  input <- readFile path
  execute (assign vars (Assign name (Value (StrVal input)))) (Lines xs)
execute vars (Lines ((OverwriteFile val file:xs))) = do
  let (StrVal path) = eval vars file
  writeFile path (getStr 1 vars (eval vars val))
  execute vars (Lines xs)
execute vars (Lines ((AppendFile val file:xs))) = do
  let (StrVal path) = eval vars file
  appendFile path (getStr 1 vars (eval vars val))
  execute vars (Lines xs)