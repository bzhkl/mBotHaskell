module Expressions (Exp, Environment, Value (..), evalExp, parseExp, lookup) where
  import Prelude hiding (lookup)
  import qualified Data.Map.Strict as Map
  import Parser
  import qualified NumberParser as NumParser
  import StringParser
  import SequenceParser
  import Interpreter
  import Control.Monad

  type Environment = Map.Map String Value
  type Name = String

  -- The 'Type' datatype represents a primitive value in our little language.
  data Value = MyInt Int | MyDec Double | MyStr String | MyBool Bool

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
  evalExp (BinOp ex1 op ex2) env  = do
    v1 <- evalExp ex1 env
    v2 <- evalExp ex2 env
    evalBinOp v1 op v2
  evalExp (Var n) env             = lookup n env

  lookup               :: Name -> Environment -> M Value
  lookup x m = case Map.lookup x m of
    Just v -> return v
    Nothing -> errorM ("Unbound variable: " ++ x)

  evalBinOp :: Value -> Op -> Value -> M Value
  evalBinOp  (MyInt a)   op (MyInt b)   = return $ MyInt  (evalIntOp a op b)
  evalBinOp  (MyDec a)   op (MyDec b)   = return $ MyDec  (evalDecOp a op b)
  evalBinOp  (MyStr a)   op (MyStr b)   = return $ MyStr  (evalStringOp a op b)
  evalBinOp  (MyBool a)  op (MyBool b)  = return $ MyBool (evalBoolOp a op b)
  evalBinOp  a           op b           = errorM ("Unable to perform operation "
                                                   ++ show op ++ " on arguments "
                                                   ++ show a ++ " and "
                                                   ++ show b ++ ".")

  evalIntOp :: Int -> Op -> Int -> Int
  evalIntOp a (:+:) b = a + b
  evalIntOp a (:-:) b = a + b
  evalIntOp a (:*:) b = a + b
  evalIntOp a (:*:) b = a `quot` b

  evalDecOp :: Double -> Op -> Double -> Double
  evalDecOp a (:+:) b = a + b
  evalDecOp a (:-:) b = a + b
  evalDecOp a (:*:) b = a + b
  evalDecOp a (:*:) b = a / b

  evalBoolOp :: Bool -> Op -> Bool -> Bool
  evalBoolOp a (:&&:) b = a && b
  evalBoolOp a (:||:) b = a || b
  evalBoolOp a (:>:)  b = a >  b
  evalBoolOp a (:<:)  b = a <  b
  evalBoolOp a (:>=:) b = a >= b
  evalBoolOp a (:<=:) b = a <= b

  evalStringOp :: String -> Op -> String -> String
  evalStringOp a (:+:) b = a ++ b

  --Parse an expression
  parseExp :: Parser Exp
  parseExp = parseDec  `mplus` parseInt   `mplus` parseString `mplus`
             parseBool `mplus` parseBinop `mplus` parseName

  --Parse a BinOp operation
  parseBinop :: Parser Exp
  parseBinop = parsePlus `mplus` parseMin `mplus` parseMul `mplus`
               parseDiv  `mplus` parseAnd `mplus` parseOr  `mplus`
               parseGt   `mplus` parseLt  `mplus` parseGte `mplus`
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

  instance Show Value where
    show (MyInt i) = show i
    show (MyBool b) = show b
    show (MyDec d) = show d
    show (MyStr s) = s
