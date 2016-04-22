import Parser
import StringParser
import NumberParser
import Control.Monad

expr :: Parser Int
expr = (do
  x <- expr
  f <- addop
  y <- factor
  return $ f x y) `mplus` factor

--expr = [f x y | x <- expr, f <- addop, y <- factor] ++ factor

addop :: Parser (Int -> Int -> Int)
addop =
  (token '+' >> return (+))
    `mplus`
  (token '-' >> return (+))

factor :: Parser Int
factor = parseNat `mplus` bracket (token '(') expr (token ')')

main :: IO ()
main = undefined
