module Main where

import Logic
import Data.Char
import Control.Applicative (Alternative, empty, (<|>), many)
import System.Environment

--PARSING--

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return = pure
  (Parser p1) >>= f =
    Parser $ \input -> do
      (input', a) <- p1 input
      runParser (f a) input'

--basic parsers
charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (rest, token)

notNull:: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

ws :: Parser String
ws = spanP isSpace

wsn :: Parser String
wsn = spanP (\c -> isSpace c && c /= '\n')

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy elem sep = (:) <$> elem <*> many (sep *> elem) <|> pure []

try :: Parser a -> Parser a
try (Parser p) =
  Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (input', x) -> Just (input', x)

option :: a -> Parser a -> Parser a
option x (Parser p) =
  Parser $ \input ->
    case p input of
      Nothing -> Just (input, x)
      Just (input', y) -> Just (input', y)

count :: Int -> Parser a -> Parser [a]
count n p
  | n <= 0 = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

lookAhead :: Parser Structure -> Parser Structure
lookAhead (Parser p) =
  Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (_, x) -> Just (input, EmptyLine)

--atom parsers
pInt :: Parser Atom
pInt = f <$> notNull (spanP isDigit)
  where
    f ds = IntVal (read ds)

pFlt :: Parser Atom
pFlt = f <$> notNull (spanP isDigit) <*> charP '.' <*> notNull (spanP isDigit)
  where
    f ds1 _ ds2 = FltVal (read (ds1 ++ "." ++ ds2))

pStr :: Parser Atom
pStr = StrVal <$> stringLiteral

pBool :: Parser Atom
pBool = f <$> (stringP "True" <|> stringP "False")
  where
    f "True" = BoolVal True
    f "False" = BoolVal False

pVName :: Parser String
pVName = spanP (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')

pList :: Parser Atom
pList = ListVal <$> (charP '[' *> ws *> sepBy parseOp (ws *> charP ',' <* ws) <* ws <* charP ']')

atom :: Parser Atom
atom = pBool <|> (VarName <$> notNull (pVName)) <|> pFlt <|> pInt <|> pStr <|> pList


--operation parsers


parseValue :: Parser Op
parseValue = (Value <$> atom) <|> (Bracket <$> (wsn *> charP '(' *> ws *> parseOp <* ws <* charP ')')) <* wsn

parseGetIndex :: Parser Op
parseGetIndex = do
  a <- parseValue
  rest <- many ((,) <$> parseIndexOp <*> parseValue)
  return $ foldl (\acc (op, val) -> op acc val) a rest

parseIndexOp :: Parser (Op -> Op -> Op)
parseIndexOp = (AccessList <$ stringP "!")

parsePow :: Parser Op
parsePow = do
  a <- parseGetIndex
  rest <- many ((,) <$> parsePowOp <*> parseValue)
  return $ foldl (\acc (op, val) -> op acc val) a rest

parsePowOp :: Parser (Op -> Op -> Op)
parsePowOp = wsn *> (Pow <$ stringP "^") <* wsn 

parseMulDiv :: Parser Op
parseMulDiv = do
  a <- parsePow
  rest <- many ((,) <$> parseMulDivOp <*> parsePow)
  return $ foldl (\acc (op, val) -> op acc val) a rest

parseMulDivOp :: Parser (Op -> Op -> Op)
parseMulDivOp = wsn *> (Mul <$ stringP "*" <|> Div <$ stringP "/" <|> Mod <$ stringP "%") <* wsn

parseAddSub :: Parser Op
parseAddSub = do
  a <- parseMulDiv
  rest <- many ((,) <$> parseAddSubOp <*> parseMulDiv)
  return $ foldl (\acc (op, val) -> op acc val) a rest

parseAddSubOp :: Parser (Op -> Op -> Op)
parseAddSubOp = wsn*> (Add <$ stringP "+" <|> Sub <$ stringP "-") <* wsn

parseComp :: Parser Op
parseComp = do
  a <- parseAddSub
  rest <- many ((,) <$> parseCompOp <*> parseAddSub)
  return $ foldl (\acc (op, val) -> op acc val) a rest

parseCompOp :: Parser (Op -> Op -> Op)
parseCompOp = wsn *> (Comp <$ stringP "==" <|> NComp <$ stringP "!=" <|> BiggerEq <$ stringP ">=" <|> SmallerEq <$ stringP "<=" 
              <|> Bigger <$ stringP ">" <|> Smaller <$ stringP "<") <* wsn

parseAnd :: Parser Op
parseAnd = do
  a <- parseComp
  rest <- many ((,) <$> parseAndOp <*> parseComp)
  return $ foldl (\acc (op, val) -> op acc val) a rest

parseAndOp :: Parser (Op -> Op -> Op)
parseAndOp = wsn *> (And <$ stringP "&&" <|> And <$ stringP "and") <* wsn

parseOr :: Parser Op
parseOr = do
  a <- parseAnd
  rest <- many ((,) <$> parseOrOp <*> parseAnd)
  return $ foldl (\acc (op, val) -> op acc val) a rest

parseOrOp :: Parser (Op -> Op -> Op)
parseOrOp = wsn *> (Or <$ stringP "||" <|> Or <$ stringP "or") <* wsn

parseOp :: Parser Op
parseOp = parseOr


--structure parsers

emptyLine :: Parser Structure
emptyLine = EmptyLine <$ (wsn *> charP '\n')

assignP :: Parser Structure
assignP = f <$> (pVName) <*> (wsn *> charP '=' *> wsn *> parseOp <* wsn <* charP '\n')
  where
    f name val = Assign name val

assignFromFile :: Parser Structure
assignFromFile = f <$> (pVName) <*> (wsn *> stringP "<-" *> wsn *> parseOp <* wsn <* charP '\n')
  where
    f name file = AssignFromFile name file

printP :: Parser Structure
printP = Print <$> (stringP ">>" *> wsn *> parseOp <* wsn <* charP '\n')

overwriteFile :: Parser Structure
overwriteFile = f <$> (parseOp) <*> (wsn *> stringP "->" *> wsn *> parseOp <* wsn <* charP '\n')
  where
    f val file = OverwriteFile val file

appendToFile :: Parser Structure
appendToFile = f <$> (parseOp) <*> (wsn *> stringP "-->" *> wsn *> parseOp <* wsn <* charP '\n')
  where
    f val file = AppendFile val file

ifElseP :: Int -> Parser Structure
ifElseP n = do
  _ <- wsn *> stringP "if" *> wsn
  condition <- parseOp
  _ <- wsn *> charP ':' *> wsn <* charP '\n'
  ifBody <- option (Lines []) (indentedLinesP (n+1))
  elseBody <- option (Lines []) (wsn *> stringP "else:" *> wsn <* charP '\n' *> indentedLinesP (n+1))
  return $ IfElse condition ifBody elseBody

indentedLineP :: Int -> Parser Structure
indentedLineP 0 = lineP 0
indentedLineP n = try (count n (stringP "  ") *> lineP n)

indentedLinesP :: Int -> Parser Structure
indentedLinesP n = Lines <$> many (try (indentedLineP n))

lineP :: Int -> Parser Structure
lineP n = emptyLine <|> assignP <|> assignFromFile <|> printP <|> overwriteFile <|> appendToFile <|> ifElseP n

runFile :: String -> IO ()
runFile fileName = do
  input <- readFile fileName
  case runParser (indentedLinesP 0) (input ++ "\n") of
    Just ("", lines) -> execute [] lines >> return ()
    Just (rest, _) -> putStrLn $ "Unparsed: " ++ rest
    Nothing -> putStrLn "Invalid input"

main:: IO()
main = do
  (name:_) <- getArgs
  runFile name