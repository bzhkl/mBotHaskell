module Expressions (evalExp, parseExp, Exp) where
  import Prelude hiding (lookup)
  import Parser
  import qualified NumberParser as NumParser
  import StringParser
  import SequenceParser
  import Interpreter

  type Environment = [(Name,Value)]
  type Name = String

  -- The 'Type' datatype represents a primitive value in our little language.
  data Value = MyInt Int | MyDec Double | MyStr String | MyBool Bool
       deriving (Show)

  -- The 'Exp' datatype represents an expression which can be evaluated.
  data Exp = Lit Value  | BinOp Exp Op Exp | Var Name
       deriving (Show)

  -- The 'Op' datatype represents a unary or BinOp operation
  data Op = (:+:)  | (:-:)  | (:*:)  |
            (:/:)  | (:&&:) | (:||:) |
            (:>:)  | (:>=:) | (:<:)  |
            (:<=:) | (:==:) | (:!=:)
            deriving (Show)

  evalExp :: Exp -> Environment -> M Value
  evalExp (Lit t) _               = return t
  evalExp (BinOp ex1 op ex2) env = do
    v1 <- evalExp ex1 env
    v2 <- evalExp ex2 env
    evalBinop v1 op v2
  evalExp (Var n) env             = lookup n env

  lookup               :: Name ->Environment -> M Value
  lookup x []          = errorM ("Unbound variable: " ++ x)
  lookup x ((y,b):e)   = if x==y then return b else lookup x e

  evalBinop :: Value -> Op -> Value -> M Value
  evalBinop (MyInt  a) (:+:)  (MyInt  b) = return $ MyInt  (a + b)
  evalBinop (MyInt  a) (:-:)  (MyInt  b) = return $ MyInt  (a - b)
  evalBinop (MyInt  a) (:*:)  (MyInt  b) = return $ MyInt  (a * b)
  evalBinop (MyInt  a) (:/:)  (MyInt  b) = return $ MyInt  (a `quot` b)
  evalBinop (MyDec  a) (:+:)  (MyDec  b) = return $ MyDec  (a + b)
  evalBinop (MyDec  a) (:-:)  (MyDec  b) = return $ MyDec  (a - b)
  evalBinop (MyDec  a) (:*:)  (MyDec  b) = return $ MyDec  (a * b)
  evalBinop (MyDec  a) (:/:)  (MyDec  b) = return $ MyDec  (a / b)
  evalBinop (MyStr  a) (:+:)  (MyStr  b) = return $ MyStr  (a ++ b)
  evalBinop (MyBool a) (:&&:) (MyBool b) = return $ MyBool (a && b)
  evalBinop (MyBool a) (:||:) (MyBool b) = return $ MyBool (a || b)
  evalBinop (MyInt  a) (:>:)  (MyInt b)  = return $ MyBool (a > b)
  evalBinop (MyInt  a) (:<:)  (MyInt b)  = return $ MyBool (a < b)
  evalBinop (MyInt  a) (:>=:) (MyInt b)  = return $ MyBool (a >= b)
  evalBinop (MyInt  a) (:<=:) (MyInt b)  = return $ MyBool (a <= b)
  evalBinop _ _ _                        = errorM "Invalid operation executed."

  --Parse an expression
  parseExp :: Parser Exp
  parseExp = parseDec +++ parseInt +++ parseString +++ parseBool +++ parseBinop +++ parseName

  --Parse a BinOp operation
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
  parseDec :: Parser Exp
  parseDec = fmap (Lit . MyDec) NumParser.parseDec

  parseName :: Parser Exp
  parseName = do
    whitespace
    name <- identifier
    whitespace
    return $ Var name

  makeBinop :: String -> Op -> Parser Exp
  makeBinop s op = do
    wToken '('
    a <- parseExp
    wMatch s
    b <- parseExp
    wToken ')'
    return $ BinOp a op b
