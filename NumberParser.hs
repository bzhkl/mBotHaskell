module NumberParser (parseNat, parseNeg, parseInt, parseDec) where
  import Parser
  import StringParser
  import SequenceParser
  import Data.Char
  import Control.Monad

  -- match a natural number
  parseNat :: Parser Int
  parseNat = do
    s <- plus (spot isDigit)
    return (read s)
  -- match a negative number
  parseNeg :: Parser Int
  parseNeg = do
    token '-'
    n <- parseNat
    return (-n)
  -- match an integer
  parseInt :: Parser Int
  parseInt = parseNat `mplus` parseNeg

  parseDec :: Parser Double
  parseDec = do
    n <- plus (spot isDigit)
    token '.'
    c <- plus (spot isDigit)
    return $ read (n ++ "." ++ c)
