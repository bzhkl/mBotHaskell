module SequenceParser (star, plus) where
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
