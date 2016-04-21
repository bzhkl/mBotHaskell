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
  data Exp = Lit Type  | Binary Exp Binop Exp
       deriving (Show, Eq)
  -- The 'Binop' datatype represents a binary operation which can be
  -- executed on an expression.
  data Binop = (:+:) | (:-:) | (:*:) | (:/:) | (:&&:) | (:||:)
       deriving (Show, Eq)

  evalExp :: Exp -> Type
  evalExp (Lit t) = t
  evalExp (Binary e1 op e2) = evalBinop (evalExp e1) op (evalExp e2)

  evalBinop :: Type -> Binop -> Type -> Type
  evalBinop (MyInt  a) (:+:)  (MyInt  b) = MyInt  (a+b)
  evalBinop (MyInt  a) (:-:)  (MyInt  b) = MyInt  (a-b)
  evalBinop (MyInt  a) (:*:)  (MyInt  b) = MyInt  (a*b)
  evalBinop (MyInt  a) (:/:)  (MyInt  b) = MyInt  (a `quot` b)
  evalBinop (MyStr  a) (:+:)  (MyStr  b) = MyStr  (a ++ b)
  evalBinop (MyBool a) (:&&:) (MyBool b) = MyBool (a && b)
  evalBinop (MyBool a) (:||:) (MyBool b) = MyBool (a || b)
  evalBinop _     _             _             = error "Invalid operation"

  --Parse an expression
  parseExp :: Parser Exp
  parseExp = parseInt `mplus` parseString `mplus` parseBool `mplus` parseBinop

  --Parse a binary operation
  parseBinop :: Parser Exp
  parseBinop = parsePlus `mplus` parseMin `mplus` parseMul `mplus`
               parseDiv  `mplus` parseAnd `mplus` parseOr

  --Parse a boolean value, ignoring whitespace
  parseBool :: Parser Exp
  parseBool = do
    s <- wMatch "true" `mplus` wMatch "false"
    return . Lit . MyBool $ s == "true"

  --Parse a string, ignoring whitespace
  parseString :: Parser Exp
  parseString = do
    _ <- whitespace >> token '"'
    str <- star $ notToken '"'
    _ <- token '"' >> whitespace
    return . Lit . MyStr $ str

  --Parse an int value, ignoring whitespace.
  parseInt :: Parser Exp
  parseInt = do
    _ <- whitespace
    n <- NumberParser.parseInt
    _ <- whitespace
    return . Lit . MyInt $ n

  --Parse the plus operator (can be used with integers and strings)
  parsePlus :: Parser Exp
  parsePlus = do
    _ <- wToken '('
    a <- parseExp
    _ <- wToken '+'
    b <- parseExp
    _ <- wToken ')'
    return $ Binary a (:+:) b

  --Parse a substraction
  parseMin :: Parser Exp
  parseMin = do
    _ <- wToken '('
    a <- parseExp
    _ <- wToken '-'
    b <- parseExp
    _ <- wToken ')'
    return $ Binary a (:-:) b

  --Parse a multiplication
  parseMul :: Parser Exp
  parseMul = do
    _ <- wToken '('
    a <- parseExp
    _ <- wToken '*'
    b <- parseExp
    _ <- wToken ')'
    return $ Binary a (:*:) b

  --Parse a division
  parseDiv :: Parser Exp
  parseDiv = do
    _ <- wToken '('
    a <- parseExp
    _ <- wToken '/'
    b <- parseExp
    _ <- wToken ')'
    return $ Binary a (:/:) b

  --Parse a boolean AND operation
  parseAnd :: Parser Exp
  parseAnd = do
    _ <- wToken '('
    a <- parseExp
    _ <- wMatch "&&"
    b <- parseExp
    _ <- wToken ')'
    return $ Binary a (:&&:) b

  --Parse a boolean NOT operation
  parseOr :: Parser Exp
  parseOr = do
    _ <- wToken '('
    a <- parseExp
    _ <- wMatch "||"
    b <- parseExp
    _ <- wToken ')'
    return $ Binary a (:||:) b
