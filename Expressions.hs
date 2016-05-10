module Expressions (Exp, Environment, Name, Value (..), evalExp, parseExp, parseExpBracket, lookup) where
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

  -- The 'Value' datatype represents a primitive value in our cute little language.
  data Value = MyInt Int | MyDec Double | MyStr String | MyBool Bool | Fun (Value -> M Value)

  -- The 'Exp' datatype represents an expression which can be evaluated.
  data Exp = Lit Value | BinOp Exp Op Exp | UnOp Op Exp | Var Name | Lam Name Exp | App Exp Exp
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
  evalExp (UnOp op ex) env        = evalExp ex env >>= \v -> evalUnOp op v
  evalExp (Var n) env             = lookup n env
  evalExp (Lam n v) env           = return $ Fun (\x -> evalExp v (Map.insert n x env) )
  evalExp (App ex1 ex2) env       = do
    lam <- evalExp ex1 env
    arg <- evalExp ex2 env
    evalFun lam arg


  lookup               :: Name -> Environment -> M Value
  lookup x m = case Map.lookup x m of
    Just v -> return v
    Nothing -> errorM ("Unbound variable: " ++ x)

  evalUnOp :: Op -> Value -> M Value
  evalUnOp (:+:) (MyInt a) = return $ MyInt a
  evalUnOp (:-:) (MyInt a) = return $ MyInt (-a)
  evalUnOp (:+:) (MyDec a) = return $ MyDec a
  evalUnOp (:-:) (MyDec a) = return $ MyDec (-a)
  evalUnOp op a            = errorM $ "Unary operation " ++ show op ++
                                    " is not supported on " ++ show a ++
                                    "."

  evalFun :: Value -> Value -> M Value
  evalFun (Fun k) a = k a
  evalFun a b = errorM  $ "Unable to apply " ++ show a ++
                       " with argument" ++ show b ++ "."

  evalBinOp :: Value -> Op -> Value -> M Value
  evalBinOp  (MyInt a)   op   (MyInt b)   = evalIntOp a op b
  evalBinOp  (MyDec a)   op   (MyDec b)   = evalDecOp a op b
  evalBinOp  (MyStr a)   op   (MyStr b)   = evalStringOp a op b
  evalBinOp  (MyBool a)  op   (MyBool b)  = evalBoolOp a op b
  evalBinOp  (MyStr a)   op   b           = evalStringOp a op (show b)
  evalBinOp  a           op   (MyStr b)   = evalStringOp (show a) op b
  evalBinOp  a           op   b           = errorM ("Unable to perform operation "
                                                   ++ show op ++ " on arguments "
                                                   ++ show a ++ " and "
                                                   ++ show b ++ ".")

  evalIntOp :: Int -> Op -> Int -> M Value
  evalIntOp a (:+:) b = return $ MyInt (a + b)
  evalIntOp a (:-:) b = return $ MyInt (a - b)
  evalIntOp a (:*:) b = return $ MyInt (a * b)
  evalIntOp a (:/:) b = return $ MyInt (a `quot` b)
  evalIntOp a (:>:)  b = return $ MyBool (a >  b)
  evalIntOp a (:<:)  b = return $ MyBool (a <  b)
  evalIntOp a (:>=:) b = return $ MyBool (a >= b)
  evalIntOp a (:<=:) b = return $ MyBool (a <= b)
  evalIntOp a op     b = errorM $ "Operation " ++ show op ++
                                  " is not supported on " ++ show a ++
                                  " and " ++ show b ++ "."

  evalDecOp :: Double -> Op -> Double -> M Value
  evalDecOp a (:+:) b = return $ MyDec (a + b)
  evalDecOp a (:-:) b = return $ MyDec (a - b)
  evalDecOp a (:*:) b = return $ MyDec (a * b)
  evalDecOp a (:/:) b = return $ MyDec (a / b)
  evalDecOp a (:>:)  b = return $ MyBool (a >  b)
  evalDecOp a (:<:)  b = return $ MyBool (a <  b)
  evalDecOp a (:>=:) b = return $ MyBool (a >= b)
  evalDecOp a (:<=:) b = return $ MyBool (a <= b)
  evalDecOp a op     b = errorM $ "Operation " ++ show op ++
                                  " is not supported on " ++ show a ++
                                  " and " ++ show b ++ "."

  evalBoolOp :: Bool -> Op -> Bool -> M Value
  evalBoolOp a (:&&:) b = return $ MyBool (a && b)
  evalBoolOp a (:||:) b = return $ MyBool (a || b)
  evalBoolOp _ _ _      = errorM "Unsupported operation on boolean values."

  evalStringOp :: String -> Op -> String -> M Value
  evalStringOp a (:+:) b = return $ MyStr (a ++ b)
  evalStringOp _ _ _     = errorM "Unsupported operation on string values."

  --Parse an expression
  parseExp :: Parser Exp
  parseExp = parseDec  `mplus` parseInt   `mplus` parseString `mplus`
             parseBool `mplus` parseBinOp `mplus` parseName `mplus`
             parseUnOp `mplus` parseLam   `mplus` parseApp

  parseExpBracket :: Parser Exp
  parseExpBracket = roundBracket parseExp

  parseUnOp :: Parser Exp
  parseUnOp = parsePlus `mplus` parseMin
              where parsePlus = makeUnOp "+" (:+:)
                    parseMin  = makeUnOp "-" (:-:)

  --Parse a BinOp operation
  parseBinOp :: Parser Exp
  parseBinOp = parsePlus `mplus` parseMin `mplus` parseMul `mplus`
               parseDiv  `mplus` parseAnd `mplus` parseOr  `mplus`
               parseGt   `mplus` parseLt  `mplus` parseGte `mplus`
               parseLte
               where parsePlus  = makeBinOp "+"   (:+:)
                     parseMin   = makeBinOp "-"   (:-:)
                     parseMul   = makeBinOp "*"   (:*:)
                     parseDiv   = makeBinOp "/"   (:/:)
                     parseAnd   = makeBinOp "&&"  (:&&:)
                     parseOr    = makeBinOp "||"  (:||:)
                     parseGt    = makeBinOp ">"   (:>:)
                     parseLt    = makeBinOp "<"   (:<:)
                     parseGte   = makeBinOp ">="  (:>=:)
                     parseLte   = makeBinOp "<="  (:<=:)

  --Parse a boolean value
  parseBool :: Parser Exp
  parseBool = do
    s <- wMatch "true" `mplus` wMatch "false"
    return . Lit . MyBool $ s == "true"

  --Parse a string
  parseString :: Parser Exp
  parseString = bracket' (token '"') (wToken '"') (do
    str <- star $ notToken '"'
    return . Lit . MyStr $ str)

  --Parse an int value
  parseInt :: Parser Exp
  parseInt = fmap (Lit . MyInt) NumParser.parseInt

  --Parse a decimal value
  parseDec :: Parser Exp
  parseDec = fmap (Lit . MyDec) NumParser.parseDec

  --Parse a variable name
  parseName :: Parser Exp
  parseName = fmap Var wIdentifier

  --Parse a lambda expression
  parseLam :: Parser Exp
  parseLam = roundBracket $ do
    name <- wIdentifier
    _ <- wMatch "=>"
    ex <- parseExp
    return $ Lam name ex

  --Parse a lambda application
  parseApp :: Parser Exp
  parseApp = roundBracket $ do
    lam <- parseExp
    arg <- roundBracket parseExp
    return $ App lam arg

  --Create a parser for a unary operation
  makeUnOp :: String -> Op -> Parser Exp
  makeUnOp s op = fmap (UnOp op) (roundBracket $ wMatch s >> parseExp)

  --Create a parser for a binary operation
  makeBinOp :: String -> Op -> Parser Exp
  makeBinOp s op = roundBracket $ do
    a <- parseExp
    _ <- wMatch s
    b <- parseExp
    return $ BinOp a op b

  instance Show Value where
    show (MyInt i) = show i
    show (MyBool b) = show b
    show (MyDec d) = show d
    show (MyStr s) = s
    show (Fun _) = "(Function)"
