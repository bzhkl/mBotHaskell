--This module is used for parsing characters and strings.
module StringParser (char, spot, token, notToken, wToken, match, whitespace, wMatch, identifier, wIdentifier, bracket, eatUntil, parseLine) where
  import Control.Monad
  import Data.Char
  import Parser
  import SequenceParser

  -- Parse one character
  char :: Parser Char
  char = Parser f
    where f [] = []
          f (c:s) = [(c,s)]

  -- Parse a character satisfying a predicate (e.g., isDigit)
  spot :: (Char -> Bool) -> Parser Char
  spot p = do
    c <- char
    guard (p c)
    return c

  -- Match a given character
  token :: Char -> Parser Char
  token c = spot (== c)

  -- Match a character not equal to a given character
  notToken :: Char -> Parser Char
  notToken c = spot (/= c)

  -- Match something between brackets
  bracket :: Parser a -> Parser b -> Parser c -> Parser b
  bracket o p c = o >> p >>= \x -> c >> return x

  --Parse a token surounded by optional whitespace
  wToken :: Char -> Parser Char
  wToken t = do
    whitespace
    c <- token t
    whitespace
    return c

  -- match a given string
  match :: String -> Parser String
  match = mapM token

  -- match a given string surounded by optional whitespace
  wMatch :: String -> Parser String
  wMatch s = do
    whitespace
    m <- match s
    whitespace
    return m

  whitespace :: Parser String
  whitespace = star (spot isSpace)

  wIdentifier :: Parser String
  wIdentifier = do
    whitespace
    x <- identifier
    whitespace
    return x

  identifier :: Parser String
  identifier = do
    x <- spot isLower
    xs <- star $ spot isAlphaNum
    return (x:xs)

  eatUntil :: String -> Parser String
  eatUntil s = wMatch s `mplus` do
    c <- char
    cs <- eatUntil s
    return (c:cs)

  parseLine :: Parser String
  parseLine = do
     c <- char;
     if c == '\n'
       then return [c]
       else parseLine >>= \s -> return (c:s)
