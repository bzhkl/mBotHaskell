--This module is used for parsing characters and strings.
module StringParser (char, spot, token, notToken, wToken, match, whitespace,
                     wMatch, identifier, wIdentifier, bracket, bracket',
                     eatUntil, parseLine, roundBracket, addWhitespace) where
  import Control.Monad
  import Data.Char
  import Data.List
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

  -- Match something between brackets, last parser's value is returned
  bracket' :: Parser a -> Parser b -> Parser c -> Parser c
  bracket' o c p = bracket o p c

  -- Match something between round brackets
  roundBracket :: Parser a -> Parser a
  roundBracket p = bracket (wToken '(') p (wToken ')')

  --Parse as much whitespace as possible
  whitespace :: Parser String
  whitespace = star $ spot isSpace

  --Change a parser so it parses optional whitespace to the right
  addWhitespace :: Parser a -> Parser a
  addWhitespace p = do
    m <- p
    _ <- whitespace
    return m

  --Parse a token with optional whitespace to the right
  wToken :: Char -> Parser Char
  wToken t = addWhitespace (token t)

  -- match a given string
  match :: String -> Parser String
  match = mapM token

  -- match a given string with optional whitespace to the right
  wMatch :: String -> Parser String
  wMatch s = addWhitespace (match s)

  -- match an identifier with optional whitespace to the right
  wIdentifier :: Parser String
  wIdentifier = addWhitespace identifier

  -- match an identifier (starts with a lowercase character and contains
  -- only alphanum characters).
  identifier :: Parser String
  identifier = do
    x <- spot isLower
    xs <- star $ spot isAlphaNum
    let name = x:xs
    guard $ checkLegalIdentifier name
    return name

  -- parse as few characters as possible until a given string is matched
  eatUntil :: String -> Parser String
  eatUntil  = helper "" . reverse
    where helper prev r = do
            c <- char
            let total = c:prev
                matched = r `isPrefixOf` total
            if matched
              then addWhitespace $ return total
              else helper total r

  -- parse until a newline character is matched
  parseLine :: Parser String
  parseLine = do
     c <- char;
     if c == '\n'
       then whitespace >> return [c]
       else parseLine >>= \s -> return (c:s)

  checkLegalIdentifier :: String -> Bool
  checkLegalIdentifier name = let reserved = ["true", "false", "let", "if", "else", "for", "while"]
                              in  name `notElem` reserved
