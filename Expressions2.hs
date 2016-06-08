module Expressions2 (Exp, Value, evalExp, parseExp) where
  import Prelude hiding (lookup)
  import Control.Monad
  import Interpreter
  import Parser
  import StringParser
  import qualified NumberParser

  data Exp    = AddOp Term Op Exp | SingleTerm Term
                deriving (Show)
  data Term   = MulOp Factor Op Term | UnOp Op Term | SingleFactor Factor
                deriving (Show)
  data Factor = SingleValue Value | Bracket Exp
                deriving (Show)
  data Value  = MyInt Int | MyBool Bool

  data Op = (:+:)  | (:-:)  | (:*:)  |
            (:/:)  | (:&&:) | (:||:) |
            (:>:)  | (:>=:) | (:<:)  |
            (:<=:) | (:==:) | (:!=:)
            deriving (Show)

  evalExp :: Exp -> M Value
  evalExp (AddOp ex1 op ex2) = do
    v1 <- evalTerm ex1
    v2 <- evalExp ex2
    evalBinOp v1 op v2
  evalExp (SingleTerm term) = evalTerm term

  evalBinOp :: Value -> Op -> Value -> M Value
  evalBinOp (MyInt a)  op (MyInt b)  = evalIntOp a op b
  evalBinOp (MyBool a) op (MyBool b) = evalBoolOp a op b
  evalBinOp  a         op b          = errorM ("Unable to perform operation "
                                                   ++ show op ++ " on arguments "
                                                   ++ show a ++ " and "
                                                   ++ show b ++ ".")

  evalUnOp :: Op -> Value -> M Value
  evalUnOp (:+:) (MyInt a) = return $ MyInt a
  evalUnOp (:-:) (MyInt a) = return $ MyInt (-a)
  evalUnOp op a            = errorM $ "Unary operation " ++ show op ++
                                    " is not supported on " ++ show a ++
                                    "."

  evalIntOp :: Int -> Op -> Int -> M Value
  evalIntOp a (:+:)  b = return $ MyInt (a + b)
  evalIntOp a (:-:)  b = return $ MyInt (a - b)
  evalIntOp a (:*:)  b = return $ MyInt (a * b)
  evalIntOp a (:/:)  b = return $ MyInt (a `quot` b)
  evalIntOp a (:>:)  b = return $ MyBool (a > b)
  evalIntOp a (:<:)  b = return $ MyBool (a < b)
  evalIntOp a (:>=:) b = return $ MyBool (a >= b)
  evalIntOp a (:<=:) b = return $ MyBool (a <= b)
  evalIntOp a op     b = errorM $ "Operation " ++ show op ++
                                  " is not supported on " ++ show a ++
                                  " and " ++ show b ++ "."

  evalBoolOp :: Bool -> Op -> Bool -> M Value
  evalBoolOp a (:&&:) b = return $ MyBool (a && b)
  evalBoolOp a (:||:) b = return $ MyBool (a || b)
  evalBoolOp _ _ _      = errorM "Unsupported operation on boolean values."

  evalTerm :: Term -> M Value
  evalTerm (MulOp fac op term) = do
    v1 <- evalFactor fac
    v2 <- evalTerm term
    evalBinOp v1 op v2
  evalTerm (SingleFactor fac) = evalFactor fac
  evalTerm (UnOp op term) = evalTerm term >>= \v -> evalUnOp op v

  evalFactor :: Factor -> M Value
  evalFactor (SingleValue v) = return v
  evalFactor (Bracket ex) = evalExp ex

  parseExp :: Parser Exp
  parseExp = parseAddOp `mplus` fmap SingleTerm parseTerm

  parseTerm :: Parser Term
  parseTerm = parseMulOp `mplus` parseUnOp `mplus` fmap SingleFactor parseFactor

  parseFactor :: Parser Factor
  parseFactor = fmap SingleValue parseValue `mplus` parseBracket

  parseAddOp :: Parser Exp
  parseAddOp = parsePlus `mplus` parseMin `mplus` parseOr  `mplus`
               parseGt   `mplus` parseLt  `mplus` parseGte `mplus`
               parseLte
               where parsePlus  = makeAddOp "+"   (:+:)
                     parseMin   = makeAddOp "-"   (:-:)
                     parseOr    = makeAddOp "||"  (:||:)
                     parseGt    = makeAddOp ">"   (:>:)
                     parseLt    = makeAddOp "<"   (:<:)
                     parseGte   = makeAddOp ">="  (:>=:)
                     parseLte   = makeAddOp "<="  (:<=:)

  parseMulOp :: Parser Term
  parseMulOp = parseMul `mplus` parseDiv  `mplus` parseAnd
              where parseMul   = makeMulOp "*"   (:*:)
                    parseDiv   = makeMulOp "/"   (:/:)
                    parseAnd   = makeMulOp "&&"  (:&&:)

  parseUnOp :: Parser Term
  parseUnOp = parsePlus `mplus` parseMin
              where parsePlus = makeUnOp "+" (:+:)
                    parseMin  = makeUnOp "-" (:-:)

  --Parse a boolean value
  parseBool :: Parser Value
  parseBool = do
    s <- wMatch "true" `mplus` wMatch "false"
    return (MyBool $ s == "true")

  --Parse an int value
  parseInt :: Parser Value
  parseInt = fmap MyInt (addWhitespace NumberParser.parseInt)

  parseValue :: Parser Value
  parseValue = parseInt `mplus` parseBool

  parseBracket :: Parser Factor
  parseBracket = fmap Bracket (roundBracket parseExp)

  --Create a parser for a unary operation
  makeUnOp :: String -> Op -> Parser Term
  makeUnOp s op = fmap (UnOp op) (wMatch s >> parseTerm)

  makeAddOp :: String -> Op -> Parser Exp
  makeAddOp s op = do
    a <- parseTerm
    _ <- wMatch s
    b <- parseExp
    return $ AddOp a op b

  makeMulOp :: String -> Op -> Parser Term
  makeMulOp s op = do
    a <- parseFactor
    _ <- wMatch s
    b <- parseTerm
    return $ MulOp a op b

  instance Show Value where
    show (MyInt i) = show i
    show (MyBool b) = show b
