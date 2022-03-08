{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Interpreter

newtype Parser a = Parser {doParse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap f' . p)
    where
      f' (a, str) = (f a, str)

instance Applicative Parser where
  pure a = Parser (\input -> Just (a, input))
  (Parser f) <*> (Parser p) =
    Parser $ \input -> do
      (f', input') <- f input
      (a, s'') <- p input'
      return (f' a, s'')

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\input -> p1 input <|> p2 input)

instance Monad Parser where
  (Parser p) >>= f =
    Parser $ \input -> do
      (a, input') <- p input
      let (Parser p') = f a
      p' input'

oneChar :: Parser Char
oneChar =
  Parser
    ( \case
        [] -> Nothing
        (c : cs) -> Just (c, cs)
    )


eof :: Char
eof = '\0'

eofP :: Parser ()
eofP = (() <$ char eof) <* lineBreak

satP :: (Char -> Bool) -> Parser Char
satP f = do
  a <- oneChar
  guard $ f a
  return a

alphaChar :: Parser Char
alphaChar = satP isAlpha

digitChar :: Parser Char
digitChar = satP isDigit

char :: Char -> Parser Char
char c = satP (== c)

space :: Parser Char
space = char ' '

lineBreak :: Parser Char
lineBreak = char '\n'

string :: String -> Parser String
string = traverse char

int :: Parser Int
int = negative <|> positive
  where
    negative = char '-' >> (* (-1)) <$> positive
    positive = read <$> some digitChar

sQuoteP :: Parser Char
sQuoteP = satP (== '\'')

identP :: Parser String
identP = sQuoteP *> ((:) <$> alphaChar <*> many (alphaChar <|> digitChar <|> char '_')) <* sQuoteP

instructionP :: String -> Parser a -> Parser a
instructionP instruction parser = do
  string instruction
  space
  parser

loadValIntP :: Parser Int
loadValIntP = instructionP "LOAD_VAL" (int <* lineBreak)

loadValP :: Parser InstsAndOps
loadValP = LoadVal <$> loadValIntP

writeVarP :: Parser InstsAndOps
writeVarP = instructionP "WRITE_VAR" (WriteVar <$> identP <* lineBreak)

readVarP :: Parser InstsAndOps
readVarP = instructionP "READ_VAR" (ReadVar <$> identP <* lineBreak)

addP :: Parser InstsAndOps
addP = string "ADD" *> (Add <$ lineBreak)

multiply :: Parser InstsAndOps
multiply = string "MULTIPLY" *> (Multiply <$ lineBreak)

returnValueP :: Parser ()
returnValueP = string "RETURN_VALUE" *> (() <$ lineBreak)

loopP :: Parser InstsAndOps
loopP = do
  string "LOOP"
  lineBreak
  insts <- many instectionsP
  string "END_LOOP"
  lineBreak
  return $ Loop insts

instectionsP :: Parser InstsAndOps
instectionsP = loadValP <|> writeVarP <|> readVarP <|> addP <|> multiply <|> loopP

byteCodeP :: Parser ByteCode
byteCodeP = do
  loadedInt <- loadValIntP
  ins <- many instectionsP
  returnValueP
  eofP
  return $ ByteCode loadedInt ins

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = dropWhileEnd isSpace

trim :: String -> String
trim = trimRight . trimLeft

parseCode :: String -> Maybe (ByteCode, String)
parseCode code = byteCode
  where
    cleanLines = unlines . (++ [[eof]]) . (trim <$>) . lines
    byteCode = doParse byteCodeP (cleanLines code)

exampleCode :: String
exampleCode =
  unlines
    [ "LOAD_VAL 1",
      "WRITE_VAR 'Y'",
      "LOAD_VAL 11",
      "LOOP",
      "WRITE_VAR 'X'",
      "READ_VAR 'X'",
      "READ_VAR 'Y'",
      "MULTIPLY",
      "WRITE_VAR 'Y'",
      "READ_VAR 'X'",
      "LOAD_VAL -1",
      "ADD",
      "END_LOOP",
      "READ_VAR 'Y'",
      "RETURN_VALUE"
    ]

errorCode :: String
errorCode =
  unlines
    [ "LOAD_VAL 1",
      "WRITE_VAR 'X'",
      "LOAD_VAL 2",
      "WRITE_VAR 'Y'",
      "READ_VAR 'Z'",
      "LOAD_VAL 1",
      "ADD",
      "READ_VAR 'X'",
      "MULTIPLY",
      "RETURN_VALUE",
      "MULTIPLY"
    ]

ins =
  unlines
    [ "WRITE_VAR 'X'",
      "LOAD_VAL 2",
      "WRITE_VAR 'Y'",
      "READ_VAR 'Z'",
      "LOAD_VAL 1",
      "ADD",
      "READ_VAR 'X'",
      "MULTIPLY"
    ]
