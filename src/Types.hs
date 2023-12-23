module Types where

data Atom
  = IntVal Int
  | FltVal Double
  | StrVal String
  | BoolVal Bool
  | ListVal [Atom]
  | Identifier String
  | FileName String
  | Null
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
  | ListComprehension Op [(String, Op)] Op
  | AccessList Op Op
  | Length Op
  | Value Atom
  | Bracket Op
  | FuncCall String [Op]
  | Ternary Op Op Op
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
  | For String Op Op Op Structure
  | ForEach String Op Structure
  | EmptyLine
  | Break
  | Function String [String] Structure
  | FCall String [Op]
  | ReturnVal Op
  | Return
  | Import String
  deriving Show