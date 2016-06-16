module Expressions (Exp, Environment, Name, Value (..), evalExp, parseExp, parseExpBracket, parseExpBracketN, lookup) where
  import Prelude hiding (lookup, getLine)
  import qualified Data.Map.Strict as Map
  import Parser
  import qualified NumberParser as NumParser
  import StringParser
  import SequenceParser
  import Control.Monad
  import MBotLibrary

  type Environment = Map.Map String Value
  type Name = String
  type IValue = Either String Value

  -- The 'Value' datatype represents a primitive value in our cute little language.
  data Value = MyInt Int | MyDec Double | MyStr String | MyBool Bool | Fun (Value -> IValue) | MyMBot Device

  -- The 'Exp' datatype represents an expression which can be evaluated.
  data Exp = Lit Value | BinOp Exp Op Exp | UnOp Op Exp | Var Name | Lam Name Exp | App Exp Exp
       deriving (Show)

  -- The 'Op' datatype represents a unary or BinOp operation
  data Op = (:+:)  | (:-:)  | (:*:)  |
            (:/:)  | (:&&:) | (:||:) |
            (:>:)  | (:>=:) | (:<:)  |
            (:<=:) | (:==:) | (:!=:) |
            (:!:)
            deriving (Show)

  --Evaluate an expression
  evalExp :: Environment -> Exp -> IValue
  evalExp _   (Lit t)        = return t
  evalExp env (BinOp a op b) = mapM (evalExp env) [a, b] >>= \[v1, v2] -> evalBinOp v1 op v2
  evalExp env (UnOp op ex)   = evalExp env ex >>= \v -> evalUnOp op v
  evalExp env (Var n)        = lookup n env
  evalExp env (Lam n v)      = return $ Fun (\x -> evalExp (Map.insert n x env) v)
  evalExp env (App a b)      = mapM (evalExp env) [a, b] >>= \[lam, arg] -> evalFun lam arg

  --Find a variable in the environment
  lookup :: Name -> Environment -> IValue
  lookup x m = case Map.lookup x m of
    Just v -> return v
    Nothing -> Left ("Unbound variable: " ++ x)

  --Evaluate a unary operation
  evalUnOp :: Op -> Value -> IValue

  evalUnOp (:+:) (MyInt a)  = return $ MyInt  a
  evalUnOp (:-:) (MyInt a)  = return $ MyInt  (-a)
  evalUnOp (:+:) (MyDec a)  = return $ MyDec  a
  evalUnOp (:-:) (MyDec a)  = return $ MyDec  (-a)
  evalUnOp (:!:) (MyBool a) = return $ MyBool (not a)
  evalUnOp op a             = Left   $ "Unary operation " ++ show op ++
                                       " is not supported on " ++ show a ++
                                       "."
  --Evaluate a function
  evalFun :: Value -> Value -> IValue
  evalFun (Fun k) a = k a
  evalFun a b = Left  $ "Unable to apply " ++ show a ++
                       " with argument" ++ show b ++ "."

  --Evaluate a binary operation
  evalBinOp :: Value -> Op -> Value -> IValue
  evalBinOp  (MyInt a)   op   (MyInt b)   = evalIntOp a op b
  evalBinOp  (MyDec a)   op   (MyDec b)   = evalDecOp a op b
  evalBinOp  (MyStr a)   op   (MyStr b)   = evalStringOp a op b
  evalBinOp  (MyBool a)  op   (MyBool b)  = evalBoolOp a op b
  evalBinOp  (MyStr a)   op   b           = evalStringOp a op (show b)
  evalBinOp  a           op   (MyStr b)   = evalStringOp (show a) op b
  evalBinOp  a           op   b           = Left ("Unable to perform operation "
                                                   ++ show op ++ " on arguments "
                                                   ++ show a ++ " and "
                                                   ++ show b ++ ".")
  --Evaluate an binary operation with int values
  evalIntOp :: Int -> Op -> Int -> IValue
  evalIntOp a (:+:) b = return $ MyInt (a + b)
  evalIntOp a (:-:) b = return $ MyInt (a - b)
  evalIntOp a (:*:) b = return $ MyInt (a * b)
  evalIntOp a (:/:) b = return $ MyInt (a `quot` b)
  evalIntOp a (:>:)  b = return $ MyBool (a >  b)
  evalIntOp a (:<:)  b = return $ MyBool (a <  b)
  evalIntOp a (:>=:) b = return $ MyBool (a >= b)
  evalIntOp a (:<=:) b = return $ MyBool (a <= b)
  evalIntOp a (:==:) b = return $ MyBool (a == b)
  evalIntOp a (:!=:) b = return $ MyBool (a /= b)
  evalIntOp a op     b = Left $ "Operation " ++ show op ++
                                  " is not supported on " ++ show a ++
                                  " and " ++ show b ++ "."

  --Evaluate a binary operation with doubles
  evalDecOp :: Double -> Op -> Double -> IValue
  evalDecOp a (:+:) b = return $ MyDec (a + b)
  evalDecOp a (:-:) b = return $ MyDec (a - b)
  evalDecOp a (:*:) b = return $ MyDec (a * b)
  evalDecOp a (:/:) b = return $ MyDec (a / b)
  evalDecOp a (:>:)  b = return $ MyBool (a >  b)
  evalDecOp a (:<:)  b = return $ MyBool (a <  b)
  evalDecOp a (:>=:) b = return $ MyBool (a >= b)
  evalDecOp a (:<=:) b = return $ MyBool (a <= b)
  evalDecOp a (:==:) b = return $ MyBool (a == b)
  evalDecOp a (:!=:) b = return $ MyBool (a /= b)
  evalDecOp a op     b = Left $ "Operation " ++ show op ++
                                  " is not supported on " ++ show a ++
                                  " and " ++ show b ++ "."

  --Evaluate a binary operation with booleans values
  evalBoolOp :: Bool -> Op -> Bool -> IValue
  evalBoolOp a (:&&:) b = return $ MyBool (a && b)
  evalBoolOp a (:||:) b = return $ MyBool (a || b)
  evalBoolOp a (:==:) b = return $ MyBool (a == b)
  evalBoolOp a (:!=:) b = return $ MyBool (a /= b)
  evalBoolOp _ _ _      = Left "Unsupported operation on boolean values."

  --Evaluate a binary operation with strings
  evalStringOp :: String -> Op -> String -> IValue
  evalStringOp a (:+:)  b  = return $ MyStr (a ++ b)
  evalStringOp a (:==:) b  = return $ MyBool (a == b)
  evalStringOp a (:!=:) b  = return $ MyBool (a /= b)
  evalStringOp _ _ _       = Left "Unsupported operation on string values."

  --Parse an expression
  parseExp :: Parser Exp
  parseExp = parseDec  `mplus` parseInt   `mplus` parseString `mplus`
             parseBool `mplus` parseBinOp `mplus` parseName `mplus`
             parseUnOp `mplus` parseLam   `mplus` parseApp

  --Parse an expression surrounded by round brackets
  parseExpBracket :: Parser Exp
  parseExpBracket = roundBracket parseExp

  --Parse a comma seperated list of expressions surrounded by
  --round brackets
  parseExpBracketN :: Int -> Parser [Exp]
  parseExpBracketN n = roundBracket $ sepByN n parseExp (wToken ',')

  parseUnOp :: Parser Exp
  parseUnOp = makeUnOp "+" (:+:) `mplus` makeUnOp "-" (:-:) `mplus`
              makeUnOp "!" (:!:)

  --Parse a BinOp operation
  parseBinOp :: Parser Exp
  parseBinOp = makeBinOp "+"  (:+:)  `mplus` makeBinOp "-"  (:-:)  `mplus`
               makeBinOp "*"  (:*:)  `mplus` makeBinOp "/"  (:/:)  `mplus`
               makeBinOp "&&" (:&&:) `mplus` makeBinOp "||" (:||:) `mplus`
               makeBinOp ">"  (:>:)  `mplus` makeBinOp "<"  (:<:)  `mplus`
               makeBinOp ">=" (:>=:) `mplus` makeBinOp "<=" (:<=:) `mplus`
               makeBinOp "==" (:==:) `mplus` makeBinOp "!=" (:!=:)

  --Parse a boolean value
  parseBool :: Parser Exp
  parseBool = do
    s <- wMatch "true" `mplus` wMatch "false"
    return . Lit . MyBool $ s == "true"

  --Parse a string
  parseString :: Parser Exp
  parseString = bracket' (token '"') (wToken '"') $
                (Lit . MyStr) <$> star (notToken '"')

  --Parse an int value
  parseInt :: Parser Exp
  parseInt = (Lit . MyInt) <$> addWhitespace NumParser.parseInt

  --Parse a decimal value
  parseDec :: Parser Exp
  parseDec = (Lit . MyDec) <$> addWhitespace NumParser.parseDec

  --Parse a variable name
  parseName :: Parser Exp
  parseName = Var <$> wIdentifier

  --Parse a lambda expression
  parseLam :: Parser Exp
  parseLam = roundBracket $ Lam <$> wIdentifier <*> (wMatch "=>" >> parseExp)

  --Parse a lambda application
  parseApp :: Parser Exp
  parseApp = roundBracket $ App <$> parseExp <*> roundBracket parseExp

  --Create a parser for a unary operation
  makeUnOp :: String -> Op -> Parser Exp
  makeUnOp s op =  UnOp op <$> roundBracket (wMatch s >> parseExp)

  --Create a parser for a binary operation
  makeBinOp :: String -> Op -> Parser Exp
  makeBinOp s op = roundBracket $ BinOp <$> parseExp
                                        <*> (wMatch s >> return op)
                                        <*> parseExp

  instance Show Value where
    show (MyInt i)  = show i
    show (MyBool b) = show b
    show (MyDec d)  = show d
    show (MyStr s)  = s
    show (MyMBot _) = "(MBot)"
    show (Fun _)    = "(Function)"
