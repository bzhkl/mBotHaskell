module Expressions (evalExp, parseExp, Exp) where
  import Prelude hiding (lookup)
  import Parser
  import qualified NumberParser as NumParser
  import StringParser
  import SequenceParser

  type Environment = [(Name,Value)]
  type Name = String

  -- The 'Type' datatype represents a primitive value in our little language.
  data Value = MyInt Int | MyDec Double | MyStr String | MyBool Bool
       deriving (Show, Eq)

  -- The 'Exp' datatype represents an expression which can be evaluated.
  data Exp = Lit Value  | Binary Exp Op Exp | Var Name
       deriving (Show, Eq)

  -- The 'Op' datatype represents a unary or binary operation
  data Op = (:+:)  | (:-:)  | (:*:)  |
            (:/:)  | (:&&:) | (:||:) |
            (:>:)  | (:>=:) | (:<:)  |
            (:<=:) | (:==:) | (:!=:)
            deriving (Show, Eq)

  evalExp :: Exp -> Environment -> Value
  evalExp (Lit t) _               = t
  evalExp (Binary ex1 op ex2) env = evalBinop (evalExp ex1 env) op (evalExp ex2 env)
  evalExp (Var n) env             = lookup n env

  lookup               :: Name ->Environment -> Value
  lookup _ []          = error "Lookup failed"
  lookup x ((y,b):e)   = if x==y then b else lookup x e

  evalBinop :: Value -> Op -> Value -> Value
  evalBinop (MyInt  a) (:+:)  (MyInt  b) = MyInt  (a + b)
  evalBinop (MyInt  a) (:-:)  (MyInt  b) = MyInt  (a - b)
  evalBinop (MyInt  a) (:*:)  (MyInt  b) = MyInt  (a * b)
  evalBinop (MyInt  a) (:/:)  (MyInt  b) = MyInt  (a `quot` b)
  evalBinop (MyDec  a) (:+:)  (MyDec  b) = MyDec  (a + b)
  evalBinop (MyDec  a) (:-:)  (MyDec  b) = MyDec  (a - b)
  evalBinop (MyDec  a) (:*:)  (MyDec  b) = MyDec  (a * b)
  evalBinop (MyDec  a) (:/:)  (MyDec  b) = MyDec  (a / b)
  evalBinop (MyStr  a) (:+:)  (MyStr  b) = MyStr  (a ++ b)
  evalBinop (MyBool a) (:&&:) (MyBool b) = MyBool (a && b)
  evalBinop (MyBool a) (:||:) (MyBool b) = MyBool (a || b)
  evalBinop (MyInt  a) (:>:)  (MyInt b)  = MyBool (a > b)
  evalBinop (MyInt  a) (:<:)  (MyInt b)  = MyBool (a < b)
  evalBinop (MyInt  a) (:>=:) (MyInt b)  = MyBool (a >= b)
  evalBinop (MyInt  a) (:<=:) (MyInt b)  = MyBool (a <= b)
  evalBinop _ _ _                        = error "Invalid operation"

  --Parse an expression
  parseExp :: Parser Exp
  parseExp = parseDec' +++ parseInt +++ parseString +++ parseBool +++ parseBinop

  --Parse a binary operation
  parseBinop :: Parser Exp
  parseBinop = parsePlus +++ parseMin +++ parseMul +++
               parseDiv  +++ parseAnd +++ parseOr  +++
               parseGt   +++ parseLt  +++ parseGte +++
               parseLte
               where parsePlus  = makeBinop "+"   (:+:)
                     parseMin   = makeBinop "-"   (:-:)
                     parseMul   = makeBinop "*"   (:*:)
                     parseDiv   = makeBinop "/"   (:/:)
                     parseAnd   = makeBinop "&&"  (:&&:)
                     parseOr    = makeBinop "||"  (:||:)
                     parseGt    = makeBinop ">"   (:>:)
                     parseLt    = makeBinop "<"   (:<:)
                     parseGte   = makeBinop ">="  (:>=:)
                     parseLte   = makeBinop "<="  (:<=:)

  --Parse a boolean value
  parseBool :: Parser Exp
  parseBool = do
    s <- wMatch "true" +++ wMatch "false"
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
  parseInt = fmap (Lit . MyInt) NumParser.parseInt

  --Parse a decimal value
  parseDec' :: Parser Exp
  parseDec' = fmap (Lit . MyDec) NumParser.parseDec

  makeBinop :: String -> Op -> Parser Exp
  makeBinop s op = do
    wToken '('
    a <- parseExp
    wMatch s
    b <- parseExp
    wToken ')'
    return $ Binary a op b
