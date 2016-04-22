module Parser (Parser (..), apply, parse) where
  import Control.Monad
  import Control.Applicative

  newtype Parser a = Parser (String -> [(a, String)])

  -- Apply a parser
  apply :: Parser a -> String -> [(a, String)]
  apply (Parser f) = f

  -- Return parsed value, assuming at least one successful parse
  parse :: Eq a => Parser a -> String -> a
  parse m s = (one . rmDup) [ x | (x,t) <- apply m s, t == "" ]
      where one [] = error "no parse"
            one [x] = x
            one xs | length xs > 1 = error "ambiguous parse"
            one _ = error "Invalid"
            rmDup [] = []
            rmDup (x:xs) = x : rmDup (filter (\y -> x /= y) xs)

  instance Functor Parser where
    fmap = liftM

  instance Applicative Parser where
    pure  = return
    (<*>) = ap

  instance Monad Parser where
    return x = Parser (\s -> [(x, s)])
    m >>= k = Parser (\s -> [ (y, u) | (x, t) <- apply m s, (y, u) <- apply (k x) t ])

  instance Alternative Parser where
    empty = Parser (const [])
    (<|>) m n = Parser (\s -> apply m s ++ apply n s)

  instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)
