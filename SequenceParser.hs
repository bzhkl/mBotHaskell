module SequenceParser (star, plus, sepBy, sepBy1) where
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

  -- recognises non-empty sequences from parser p, seperated by
  -- sequences of parser sep
  sepBy1 :: Parser a -> Parser b -> Parser [a]
  sepBy1 p sep = do
    x <- p
    xs <- plus $ sep >> p
    return (x:xs)
