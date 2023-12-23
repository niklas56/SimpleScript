module Parser where

import Types
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

condP :: (Char -> Bool) -> Parser Char
condP f = Parser $ \input ->
  case input of
    (x:xs)
      | f x -> Just (xs, x)
      | otherwise -> Nothing
    _ -> Nothing

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

escapeP :: Parser Char
escapeP = do
  _ <- charP '\\'
  c <- condP (\c -> c == '"' || c == '\\' || c == 'n' || c == 't')
  case c of
    '"' -> return '"'
    '\\' -> return '\\'
    'n' -> return '\n'
    't' -> return '\t'
    _ -> empty

stringLiteral :: Parser String
stringLiteral = charP '"' *> many (escapeP <|> condP (/= '"')) <* charP '"'

fileNameP :: Parser String
fileNameP = spanP (\c -> c /= ' ' && c /= '\n')

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
pBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = BoolVal True
    f "false" = BoolVal False

pIdentifier :: Parser String
pIdentifier = spanP (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9')

pNull :: Parser Atom
pNull = Null <$ stringP "null"

atom :: Parser Atom
atom = pNull <|> pBool <|> pFlt <|> pInt <|> pStr <|> (Identifier <$> notNull (pIdentifier))




--operation parsers

pList :: Parser Op
pList = CreateList <$> (charP '[' *> ws *> sepBy parseOp (ws *> charP ',' <* ws) <* ws <* charP ']')

pListCompW :: Parser Op
pListCompW = ListComprehension <$> (charP '[' *> wsn*> parseOp) <*> (wsn *> charP '|' *> wsn *> sepBy (tuple <$> pIdentifier <*> (wsn *> stringP "<-" *> wsn *> parseOp)) (wsn *> charP ';' <* wsn) <* wsn <* charP '|') <*> (wsn *> parseOp <* wsn <* charP ']')
  where
    tuple name val = (name, val)

pListComp :: Parser Op 
pListComp = f <$> (charP '[' *> wsn*> parseOp) <*> (wsn *> charP '|' *> wsn *> sepBy (tuple <$> pIdentifier <*> (wsn *> stringP "<-" *> wsn *> parseOp)) (wsn *> charP ';' <* wsn) <* wsn <* charP ']')
  where
    tuple name val = (name, val)
    f val list = ListComprehension val list (Value (BoolVal True))

pTernary :: Parser Op
pTernary = Ternary <$> (simpleValue <* wsn <* charP '?') <*> (wsn *> parseOp <* wsn <* charP ':') <*> (wsn *> parseOp)

pLength :: Parser Op
pLength = Length <$> (charP '#' *> parseValue)

pNot :: Parser Op
pNot = Not <$> (charP '!'*> parseOp)

pNegative :: Parser Op
pNegative = (Sub (Value (IntVal 0))) <$> (charP '-' *> parseOp)

pParams :: Parser [Op]
pParams = charP '(' *> ws *> sepBy parseOp (ws *> charP ',' <* ws) <* ws <* charP ')'

pFuncCall :: Parser Op
pFuncCall = FuncCall <$> (notNull pIdentifier) <*> pParams

pDotFuncCall :: Parser Op
pDotFuncCall = f <$> simpleValue <*> (charP '.' *> ws *> notNull pIdentifier) <*> pParams
  where
    f val name params = FuncCall name (val:params)

simpleValue:: Parser Op
simpleValue = pFuncCall <|> (Value <$> atom) <|> pListCompW <|> pListComp <|> pList <|> pLength <|> pNot <|> pNegative <|> (Bracket <$> (wsn *> charP '(' *> ws *> parseOp <* ws <* charP ')')) <* wsn

parseValue :: Parser Op
parseValue = pDotFuncCall <|> pFuncCall <|> pTernary <|> (Value <$> atom) <|> pListCompW <|> pListComp <|> pList <|> pLength <|> pNot <|> pNegative <|> (Bracket <$> (wsn *> charP '(' *> ws *> parseOp <* ws <* charP ')')) <* wsn

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

comment :: Parser String
comment = (stringP "//" <|> stringP "#!") *> spanP (/= '\n') <* stringP "\n" --comment and ignore shebang

emptyLine :: Parser Structure
emptyLine = EmptyLine <$ (wsn *> (stringP "\n" <|> comment))

importP :: Parser Structure
importP = Import <$> (stringP "import" *> wsn *> fileNameP <* wsn <* (stringP "\n" <|> comment))

assignP :: Parser Structure
assignP = f <$> (pIdentifier) <*> (wsn *> charP '=' *> wsn *> parseOp <* wsn <* (stringP "\n" <|> comment))
  where
    f name val = Assign name val

assignFromFile :: Parser Structure
assignFromFile = f <$> (pIdentifier) <*> (wsn *> stringP "<-" *> wsn *> parseOp <* wsn <* (stringP "\n" <|> comment))
  where
    f name file = AssignFromFile name file

printP :: Parser Structure
printP = Print <$> (stringP ">>" *> wsn *> parseOp <* wsn <* (stringP "\n" <|> comment))

overwriteFile :: Parser Structure
overwriteFile = f <$> (parseOp) <*> (wsn *> stringP "->" *> wsn *> parseOp <* wsn <* (stringP "\n" <|> comment))
  where
    f val file = OverwriteFile val file

appendToFile :: Parser Structure
appendToFile = f <$> (parseOp) <*> (wsn *> stringP "-->" *> wsn *> parseOp <* wsn <* (stringP "\n" <|> comment))
  where
    f val file = AppendFile val file

ifElseP :: Int -> Parser Structure
ifElseP n = do
  _ <- stringP "if" *> wsn
  condition <- parseOp
  _ <- wsn *> charP ':' *> wsn <* (stringP "\n" <|> comment)
  ifBody <- option (Lines []) (indentedLinesP (n+1))
  elseBody <- option (Lines []) ((wsn*> stringP "else:" *> wsn *> try (stringP "\n" <|> comment)) *> indentedLinesP (n+1))
  return $ IfElse condition ifBody elseBody

whileP :: Int -> Parser Structure
whileP n = do
  _ <- stringP "while" *> wsn
  condition <- parseOp
  _ <- wsn *> charP ':' *> wsn <* (stringP "\n" <|> comment)
  body <- option (Lines []) (indentedLinesP (n+1))
  return $ While condition body

forP :: Int -> Parser Structure
forP n = do
  _ <- stringP "for" *> wsn
  name <- pIdentifier <* wsn
  initial <- charP '=' *> wsn *> parseOp <* wsn
  condition <- charP ';' *> wsn *> parseOp <* wsn
  increment <- charP ';' *> wsn *> parseOp <* wsn
  _ <- charP ':' *> wsn <* (stringP "\n" <|> comment)
  body <- option (Lines []) (indentedLinesP (n+1))
  return $ For name initial condition increment body

forEachP :: Int -> Parser Structure
forEachP n = do
  _ <- stringP "for" *> wsn
  name <- pIdentifier <* wsn
  list <- stringP "<-" *> wsn *> parseOp <* wsn
  _ <- charP ':' *> wsn <* (stringP "\n" <|> comment)
  body <- option (Lines []) (indentedLinesP (n+1))
  return $ ForEach name list body
 
funcP :: Parser Structure
funcP = do
  _ <- stringP "fn" *> wsn
  name <- pIdentifier <* wsn
  params <- charP '(' *> wsn *> sepBy pIdentifier (wsn *> charP ',' <* wsn) <* wsn <* charP ')'
  _ <- wsn *> charP ':' *> wsn <* (stringP "\n" <|> comment)
  body <- option (Lines []) (indentedLinesP 1)
  return $ Function name params body

pJustFunc :: Parser Structure
pJustFunc = FCall <$> (notNull pIdentifier) <*> pParams <* (stringP "\n" <|> comment)

pDotJustFunc :: Parser Structure
pDotJustFunc = f <$> simpleValue <*> (charP '.' *> ws *> notNull pIdentifier) <*> pParams <* (stringP "\n" <|> comment)
  where
    f val name params = FCall name (val:params)

breakP :: Parser Structure
breakP = Break <$ (stringP "break" <* wsn <* (stringP "\n" <|> comment))

returnP :: Parser Structure
returnP = ReturnVal <$> (stringP "return" *> wsn *> parseOp <* wsn <* (stringP "\n" <|> comment)) <|> Return <$ (stringP "return" <* wsn <* (stringP "\n" <|> comment))

indentedLineP :: Int -> Parser Structure
indentedLineP 0 = lineP 0
indentedLineP n = try (count n (stringP "   ") *> lineP n)

indentedLinesP :: Int -> Parser Structure
indentedLinesP n = Lines <$> many (try (indentedLineP n))

lineP :: Int -> Parser Structure
lineP n = emptyLine <|> importP <|> assignP <|> assignFromFile <|> printP <|> overwriteFile <|> appendToFile <|> ifElseP n <|> whileP n <|> forP n <|> forEachP n <|> funcP <|> pJustFunc <|> pDotJustFunc <|> breakP <|> returnP
