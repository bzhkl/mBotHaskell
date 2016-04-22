module Expressions (evalExp, parseExp) where
  import Control.Monad
  import Parser
  import qualified NumberParser
  import StringParser
  import SequenceParser

  -- The 'Type' datatype represents a primitive value in our little language.
  data Type = MyInt Int | MyDecimal Double | MyStr String | MyBool Bool
       deriving (Show, Eq)

  -- The 'Exp' datatype represents an expression which can be evaluated.
  data Exp = Lit Type  | Binary Exp Op Exp
       deriving (Show, Eq)

  -- The 'Op' datatype represents a unary or binary operation
  data Op = (:+:) | (:-:) | (:*:) | (:/:) | (:&&:) | (:||:)
       deriving (Show, Eq)

  evalExp :: Exp -> Type
  evalExp (Lit t) = t
  evalExp (Binary e1 op e2) = evalBinop (evalExp e1) op (evalExp e2)

  evalBinop :: Type -> Op -> Type -> Type
  evalBinop (MyInt  a) (:+:)  (MyInt  b) = MyInt  (a+b)
  evalBinop (MyInt  a) (:-:)  (MyInt  b) = MyInt  (a-b)
  evalBinop (MyInt  a) (:*:)  (MyInt  b) = MyInt  (a*b)
  evalBinop (MyInt  a) (:/:)  (MyInt  b) = MyInt  (a `quot` b)
  evalBinop (MyStr  a) (:+:)  (MyStr  b) = MyStr  (a ++ b)
  evalBinop (MyBool a) (:&&:) (MyBool b) = MyBool (a && b)
  evalBinop (MyBool a) (:||:) (MyBool b) = MyBool (a || b)
  evalBinop _ _ _                        = error "Invalid operation"

  --Parse an expression
  parseExp :: Parser Exp
  parseExp = parseInt `mplus` parseString `mplus` parseBool `mplus` parseBinop

  --Parse a binary operation
  parseBinop :: Parser Exp
  parseBinop = parsePlus `mplus` parseMin `mplus` parseMul `mplus`
               parseDiv  `mplus` parseAnd `mplus` parseOr

  --Parse a boolean value
  parseBool :: Parser Exp
  parseBool = do
    s <- wMatch "true" `mplus` wMatch "false"
    return . Lit . MyBool $ s == "true"

  --Parse a string
  parseString :: Parser Exp
  parseString = do
    token '"'
    str <- star $ notToken '"'
    token '"'
    return . Lit . MyStr $ str

  --Parse an int value
  parseInt :: Parser Exp
  parseInt = fmap (Lit . MyInt) NumberParser.parseInt

  --Parse the plus operator (can be used with integers and strings)
  parsePlus :: Parser Exp
  parsePlus = makeBinop "+" (:+:)

  --Parse a substraction
  parseMin :: Parser Exp
  parseMin = makeBinop "-" (:-:)

  --Parse a multiplication
  parseMul :: Parser Exp
  parseMul = makeBinop "*" (:*:)

  --Parse a division
  parseDiv :: Parser Exp
  parseDiv = makeBinop "/" (:/:)

  --Parse a boolean AND operation
  parseAnd :: Parser Exp
  parseAnd = makeBinop "&&" (:&&:)

  --Parse a boolean NOT operation
  parseOr :: Parser Exp
  parseOr = makeBinop "||" (:||:)

  makeBinop :: String -> Op -> Parser Exp
  makeBinop s op = do
    token '('
    a <- parseExp
    match s
    b <- parseExp
    token ')'
    return $ Binary a op b

  chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
  chainl p op a = (p `chainl1` op) `mplus` return a

  chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
  p `chainl1` op = do {a <- p; rest a}
    where
      rest a = (do
        f <- op
        b <- p
        rest (f a b))
        `mplus` return a
