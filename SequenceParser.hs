module SequenceParser (star, plus, sepBy, sepByN) where
  import Parser
  import Control.Monad
  -- match zero or more occurrences
  star :: Parser a -> Parser [a]
  star p = plus p `mplus` return []
  -- match one or more occurrences
  plus :: Parser a -> Parser [a]
  plus p = do
    x <- p
    xs <- star p
    return (x:xs)

  -- recognises sequences from parser p, seperated by
  -- sequences of parser sep
  sepBy :: Parser a -> Parser b -> Parser [a]
  sepBy p sep = do
    x <- p
    xs <- star $ sep >> p
    return (x:xs)

  -- parse a sequence of n time parser p seperated by
  -- parser sep
  sepByN :: Int -> Parser a -> Parser b -> Parser [a]
  sepByN n p sep = do
    x <- p
    xs <- replicateM (n - 1) (sep >> p)
    return (x:xs)
