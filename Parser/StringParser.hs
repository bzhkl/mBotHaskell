--This module is used for parsing characters and strings.
module StringParser (char, spot, token, notToken, wToken, match, whitespace, wMatch) where
  import Control.Monad
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

  --Parse a token surounded by optional whitespace
  wToken :: Char -> Parser Char
  wToken t = do
    _ <- whitespace
    c <- token t
    _ <- whitespace
    return c

  -- match a given string
  match :: String -> Parser String
  match = mapM token

  -- match a given string surounded by optional whitespace
  wMatch :: String -> Parser String
  wMatch s = do
    _ <- whitespace
    m <- match s
    _ <- whitespace
    return m

  whitespace :: Parser String
  whitespace = star (token ' ')
